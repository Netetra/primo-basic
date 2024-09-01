use std::collections::HashMap;

use crate::ast::{
    Assign, Block, Component, Elif, Expr, FuncCall, FuncDef, If, Pair, Program, Stmt, Value, VarDef,
};

pub type EvalError = String;
pub type VarStore = HashMap<String, Value>;
pub type FuncStore = HashMap<String, Func>;
pub type ReturnType = Option<Value>;

#[derive(Debug)]
pub struct Func {
    arg_idents: Vec<String>,
    block: Block,
}

enum Event {
    Next,
    Break,
    Return(ReturnType),
    None,
}

#[derive(Debug)]
pub struct Evaluator {
    func_store: FuncStore,
}

impl Evaluator {
    pub fn new(ast: &Program) -> Result<Evaluator, EvalError> {
        let mut evaluator = Evaluator {
            func_store: FuncStore::new(),
        };
        for component in ast {
            match component {
                Component::FuncDef(func_def) => Self::eval_func_def(&mut evaluator, func_def)?,
            }
        }
        Ok(evaluator)
    }
    pub fn eval(&self, entry_point: &str) -> Result<ReturnType, String> {
        let var_store = VarStore::new();
        let arg_exprs: Vec<Expr> = vec![];

        match self.func_store.get(entry_point) {
            Some(Func { arg_idents, block }) => {
                let args = self.args_zip(&var_store, arg_idents, &arg_exprs)?;
                self.eval_func(&args, block)
            }
            None => Err("main function not found.".to_string()),
        }
    }
    fn eval_func(&self, args: &VarStore, block: &Block) -> Result<ReturnType, EvalError> {
        let mut var_store = args.clone();
        let event = self.eval_block(&mut var_store, block)?;

        match event {
            Event::Next | Event::Break => Err("".to_string()),
            Event::Return(v) => Ok(v),
            Event::None => Ok(None),
        }
    }
    fn eval_loop(&self, var_store: &mut VarStore, block: &Block) -> Result<Event, EvalError> {
        'Loop: loop {
            let event = self.eval_block(var_store, block)?;
            match event {
                Event::Next => continue 'Loop,
                Event::Break => break 'Loop,
                Event::Return(v) => return Ok(Event::Return(v)),
                Event::None => {}
            }
        }
        Ok(Event::None)
    }
    fn eval_block(&self, var_store: &mut VarStore, block: &Block) -> Result<Event, EvalError> {
        for stmt in block {
            let event = self.eval_stmt(var_store, stmt)?;
            match event {
                Event::Next => return Ok(Event::Next),
                Event::Break => return Ok(Event::Break),
                Event::Return(v) => return Ok(Event::Return(v)),
                Event::None => {}
            }
        }
        Ok(Event::None)
    }
    fn eval_stmt(&self, var_store: &mut VarStore, stmt: &Stmt) -> Result<Event, EvalError> {
        match stmt {
            Stmt::VarDef(VarDef { name, expr: value }) => {
                self.eval_var_def(var_store, name, self.eval_expr(&var_store, value)?)?;
                Ok(Event::None)
            }
            Stmt::Assign(Assign { name, expr: value }) => {
                self.eval_assign(var_store, name, self.eval_expr(&var_store, value)?)?;
                Ok(Event::None)
            }
            Stmt::If(i) => self.eval_if(var_store, i),
            Stmt::Print(args) => {
                self.eval_print(&var_store, args)?;
                Ok(Event::None)
            }
            Stmt::Loop(block) => self.eval_loop(var_store, block),
            Stmt::Next => Ok(Event::Next),
            Stmt::Break => Ok(Event::Break),
            Stmt::Expr(e) => {
                let _ = self.eval_expr(&var_store, e)?;
                Ok(Event::None)
            }
            Stmt::Return(e) => {
                if let Some(e) = e {
                    let value = self.eval_expr(&var_store, e)?;
                    return Ok(Event::Return(Some(value)));
                }
                return Ok(Event::Return(None));
            }
        }
    }
    fn eval_if(&self, var_store: &mut VarStore, r#if: &If) -> Result<Event, EvalError> {
        let If {
            condition,
            block,
            elif_blocks,
            else_block,
        } = r#if;

        if self.eval_expr(&var_store, &condition)?.into_bool()? {
            return self.eval_block(var_store, &block);
        }
        if let Some(elif_blocks) = elif_blocks {
            for elif_block in elif_blocks {
                let Elif { condition, block } = elif_block;
                if self.eval_expr(&var_store, &condition)?.into_bool()? {
                    return self.eval_block(var_store, &block);
                }
            }
        }
        if let Some(else_block) = else_block {
            return self.eval_block(var_store, &else_block);
        }

        Ok(Event::None)
    }
    fn eval_func_def(&mut self, func_def: &FuncDef) -> Result<(), EvalError> {
        let FuncDef {
            name,
            arg_idents,
            block,
        } = func_def;
        if self.func_store.get(name).is_some() {
            return Err("".to_string());
        }
        let func = Func {
            arg_idents: arg_idents.clone(),
            block: block.clone(),
        };
        self.func_store.insert(name.clone(), func);
        Ok(())
    }

    fn eval_var_def(
        &self,
        var_store: &mut VarStore,
        name: &String,
        value: Value,
    ) -> Result<(), EvalError> {
        if var_store.get(name).is_some() {
            return Err("".to_string());
        }
        var_store.insert(name.clone(), value);
        Ok(())
    }

    fn eval_assign(
        &self,
        var_store: &mut VarStore,
        name: &String,
        value: Value,
    ) -> Result<(), EvalError> {
        if var_store.get(name).is_none() {
            return Err("".to_string());
        }
        var_store.insert(name.clone(), value);
        Ok(())
    }

    fn eval_print(&self, var_store: &VarStore, args: &Vec<Expr>) -> Result<(), EvalError> {
        for arg in args {
            match self.eval_expr(var_store, arg)? {
                Value::Bool(v) => print!("{}", v),
                Value::Int(v) => print!("{}", v),
                Value::Str(v) => print!("{}", v),
                Value::None => print!(""),
            }
        }
        Ok(())
    }

    pub fn eval_expr(&self, var_store: &VarStore, expr: &Expr) -> Result<Value, EvalError> {
        match expr {
            Expr::Not(e) => Ok(Value::Bool(!self.eval_expr(var_store, e)?.into_bool()?)),
            Expr::And(Pair { lhs, rhs }) => Ok(Value::Bool(
                self.eval_expr(var_store, lhs)?.into_bool()?
                    && self.eval_expr(var_store, rhs)?.into_bool()?,
            )),
            Expr::Or(Pair { lhs, rhs }) => Ok(Value::Bool(
                self.eval_expr(var_store, lhs)?.into_bool()?
                    || self.eval_expr(var_store, rhs)?.into_bool()?,
            )),
            Expr::Eq(Pair { lhs, rhs }) => Ok(Value::Bool(
                self.eval_expr(var_store, lhs)? == self.eval_expr(var_store, rhs)?,
            )),
            Expr::NotEq(Pair { lhs, rhs }) => Ok(Value::Bool(
                self.eval_expr(var_store, lhs)? != self.eval_expr(var_store, rhs)?,
            )),
            Expr::Add(Pair { lhs, rhs }) => Ok(Value::Int(
                self.eval_expr(var_store, lhs)?.into_int()?
                    + self.eval_expr(var_store, rhs)?.into_int()?,
            )),
            Expr::Sub(Pair { lhs, rhs }) => Ok(Value::Int(
                self.eval_expr(var_store, lhs)?.into_int()?
                    - self.eval_expr(var_store, rhs)?.into_int()?,
            )),
            Expr::Mul(Pair { lhs, rhs }) => Ok(Value::Int(
                self.eval_expr(var_store, lhs)?.into_int()?
                    * self.eval_expr(var_store, rhs)?.into_int()?,
            )),
            Expr::Div(Pair { lhs, rhs }) => Ok(Value::Int(
                self.eval_expr(var_store, lhs)?.into_int()?
                    / self.eval_expr(var_store, rhs)?.into_int()?,
            )),
            Expr::Mod(Pair { lhs, rhs }) => Ok(Value::Int(
                self.eval_expr(var_store, lhs)?.into_int()?
                    % self.eval_expr(var_store, rhs)?.into_int()?,
            )),
            Expr::Expr(e) => self.eval_expr(var_store, e),
            Expr::FuncCall(FuncCall { name, arg_exprs }) => {
                let Func { arg_idents, block } = match self.func_store.get(name) {
                    Some(f) => f,
                    None => return Err("".to_string()),
                };
                let args = self.args_zip(var_store, arg_idents, arg_exprs)?;
                let result = self.eval_func(&args, block)?;
                match result {
                    Some(v) => Ok(v),
                    None => Ok(Value::None),
                }
            }
            Expr::Literal(v) => Ok(v.clone()),
            Expr::Ident(name) => match var_store.get(name) {
                Some(v) => Ok(v.clone()),
                None => Err("".to_string()),
            },
        }
    }
    fn args_zip(
        &self,
        var_store: &VarStore,
        arg_idents: &Vec<String>,
        arg_exprs: &Vec<Expr>,
    ) -> Result<VarStore, EvalError> {
        if arg_idents.len() != arg_exprs.len() {
            return Err("".to_string());
        }
        let arg_values = arg_exprs
            .into_iter()
            .map(|expr| self.eval_expr(var_store, expr))
            .collect::<Result<Vec<Value>, EvalError>>()?;
        let args = arg_idents
            .clone()
            .into_iter()
            .zip(arg_values.into_iter())
            .collect();
        Ok(args)
    }
}

impl Value {
    pub fn into_bool(self) -> Result<bool, EvalError> {
        match self {
            Value::Bool(v) => Ok(v),
            Value::Int(v) => Ok(v != 0),
            Value::Str(_) => Err("<str> cant into <bool>".to_string()),
            Value::None => Err("<none> cannot into <bool>".to_string()),
        }
    }
    pub fn into_int(self) -> Result<i64, EvalError> {
        match self {
            Value::Bool(v) => Ok(v as i64),
            Value::Int(v) => Ok(v),
            Value::Str(_) => Err("<str> cant into <int>".to_string()),
            Value::None => Err("<none> cant into <int>".to_string()),
        }
    }
}
