use crate::types::{Ast, Type, BinOp, Storage, Env};

use nom::character::complete::{
    char,
    digit1,
    alpha1,
    alphanumeric0,
    multispace0,
    multispace1,
    one_of,
};
use nom::bytes::complete::{
    tag,
    take_until,
};
use nom::branch::alt;
use nom::sequence::tuple;
use nom::multi::many0;
use nom::IResult;

impl Ast {
    fn lit_var(input: &str) -> IResult<&str, Ast> {
        let (input, ident) = Ast::identifier(input)?;
        Ok((input, Ast::Literal { ty: Type::Var, storage: Storage::Stack, content: ident }))
    }

    fn lit_u32(input: &str) -> IResult<&str, Ast> {
        let (input, num) = digit1(input)?;
        Ok((input, Ast::Literal { ty: Type::U32, storage: Storage::Direct, content: num.to_string() }))
    }

    fn lit_bool(input: &str) -> IResult<&str, Ast> {
        let (input, cond) = alt((tag("true"), tag("false")))(input)?;
        Ok((input, Ast::Literal { ty: Type::Bool, storage: Storage::Direct, content: cond.to_string() }))
    }

    fn literal(input: &str) -> IResult<&str, Ast> {
        alt((
            Ast::lit_u32,
            Ast::lit_bool,
            Ast::lit_var,
        ))(input)
    }

    fn ty_u32(input: &str) -> IResult<&str, Type> {
        let (input, _) = tag("u32")(input)?;
        Ok((input, Type::U32))
    }

    fn ty_bool(input: &str) -> IResult<&str, Type> {
        let (input, _) = tag("bool")(input)?;
        Ok((input, Type::Bool))
    }

    fn ty_void(input: &str) -> IResult<&str, Type> {
        let (input, _) = tag("void")(input)?;
        Ok((input, Type::Void))
    }

    fn ty(input: &str) -> IResult<&str, Type> {
        alt((
            Ast::ty_u32,
            Ast::ty_bool,
            Ast::ty_void,
        ))(input)
    }

    fn identifier(input: &str) -> IResult<&str, String> {
        let (input, (prefix, body)) = tuple((alpha1, alphanumeric0))(input)?;
        Ok((input, format!("{prefix}{body}")))
    }

    fn bin_op(input: &str) -> IResult<&str, Ast> {
        let (input, (c, _, left, _, right)) = tuple((
            one_of("+-*/"),
            multispace1,
            Ast::expr,
            multispace1,
            Ast::expr,
        ))(input)?;

        let bin_op = match c {
            '+' => BinOp::Add,
            '-' => BinOp::Sub,
            '*' => BinOp::Mul,
            '/' => BinOp::Div,
            _ => unreachable!()
        };

        Ok((input, Ast::BinOp { bin_op, left: Box::new(left), right: Box::new(right) }))
    }

    fn block(input: &str) -> IResult<&str, Ast> {
        let (input, (_, _, nodes, _)) = tuple((
                char('{'),
                multispace0,
                many0(tuple((
                    alt((
                        Ast::assignment,
                        Ast::expr,
                    )),
                    multispace0,
                    char(';'),
                    multispace0,
                ))),
                char('}'),
        ))(input)?;

        Ok((input, Ast::Block { ty: Type::Void, nodes: nodes.into_iter().map(|(ast, _, _, _)| ast).collect() }))
    }

    fn expr(input: &str) -> IResult<&str, Ast> {
        alt((
            Ast::bin_op,
            Ast::literal,
            Ast::block,
        ))(input)
    }

    fn assignment(input: &str) -> IResult<&str, Ast> {
        let (input, (ty, _, identifier, _, _eq, _, expr)) = tuple((
            Ast::ty,
            multispace1,
            Ast::identifier,
            multispace0,
            char('='),
            multispace0,
            Ast::expr,
        ))(input)?;

        Ok((input, Ast::Assign { ty, variable: identifier, expr: Box::new(expr) }))
    }

    pub fn check_type(&self, env: &mut Env) -> Type {
        match self {
            Ast::Block { ty, nodes } => {
                let mut scope = env.clone();
                for node in nodes {
                    node.check_type(&mut scope);
                }
                *ty
            },
            Ast::Literal { ty, storage: _, content } => match ty {
                Type::Var => *env.types.get(content).unwrap_or_else(|| panic!("Variable {content:?}'s type undefined")),
                ty => *ty,
            },
            Ast::BinOp { bin_op: _, left, right } => {
                assert_eq!(left.check_type(env), right.check_type(env));

                left.check_type(env)
            }
            Ast::Assign { ty, variable, expr } => {
                env.types.insert(variable.to_owned(), *ty);

                let exty = expr.check_type(env);
                assert_eq!(*ty, exty);

                *ty
            },
        }
    }

    fn comment(input: &str) -> IResult<&str, &str> {
        let (input, (_, body, _)) = tuple((
            tag("//"),
            take_until("\n"),
            tag("\n"),
        ))(input)?;

        Ok((input, body))
    }

    pub fn parse(mut input: &str) -> IResult<&str, Vec<Ast>> {
        let mut nodes = vec![];
        input = input.trim_start();

        while !input.is_empty() {
            if let Ok((output, _)) = Ast::comment(input) {
                input = output.trim_start();
                continue;
            }

            let (output, (ast, _, _sq)) = tuple((
                alt((
                    Ast::assignment,
                    Ast::expr,
                )),
                multispace0,
                char(';'),
            ))(input)?;

            input = output.trim_start();
            nodes.push(ast);
        }

        Ok((input, nodes))
    }

    pub fn assemble(&self, env: &mut Env) -> String {
        let mut output = String::new();

        match self {
            Ast::Block { ty: _, nodes } => {
                let mut scope = env.clone();
                for node in nodes {
                    output += &node.assemble(&mut scope);
                }
            },
            Ast::Literal { ty, storage, content } => {
                output += &match ty {
                    Type::Var => match env.types.get(content).unwrap_or_else(|| panic!("Variable {content:?}'s type undefined")) {
                        Type::Var => unreachable!("Var type cannot contain another Var"),
                        Type::Void => unimplemented!("Should've been removed by optimizer"),
                        Type::U32 => format!("mov eax, dword[ebp-{}]\n", env.stack.get(content).unwrap_or_else(|| panic!("Variable {content:?}'s stack location undefined"))),
                        Type::Bool => format!("movsx eax, byte[ebp-{}]\n", env.stack.get(content).unwrap_or_else(|| panic!("Variable {content:?}'s stack location undefined"))),
                    },
                    Type::Void => unimplemented!("Should've been removed by optimizer"),
                    Type::U32 => format!("mov eax, {content}\n"),
                    Type::Bool => format!("mov eax, {content}\n"),
                };
            },
            Ast::BinOp { bin_op, left, right } => {
                output += &left.assemble(env);
                output += "mov edi, eax\n";
                output += &right.assemble(env);
                output += "mov esi, eax\n";

                match bin_op {
                    BinOp::Add => {
                        output += "add edi, esi\n";
                        output += "mov eax, edi\n";
                    },
                    BinOp::Sub => {
                        output += "sub edi, esi\n";
                        output += "mov eax, edi\n";
                    },
                    BinOp::Mul => {
                        output += "mov eax, edi\n";
                        output += "mov edx, esi\n";
                        output += "mul edx\n";
                    },
                    BinOp::Div => {
                        output += "xor edx, edx\n";
                        output += "mov eax, edi\n";
                        output += "mov edx, esi\n";
                        output += "div edx\n";
                    },
                }
            },
            Ast::Assign { ty, variable, expr } => {
                env.stack.insert(variable.to_owned(), env.ssize);
                env.ssize += ty.size();
                env.types.insert(variable.to_owned(), *ty);

                output += &expr.assemble(env);
                output += &format!("mov [ebp-{}], eax\n", env.stack.get(variable).unwrap());
            },
        }

        output
    }
}
