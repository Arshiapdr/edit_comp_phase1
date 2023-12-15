#include "Parser.h"

// main point is that the whole input has been consumed
AST *Parser::parse()
{
    AST *Res = parseGSM();
    return Res;
}

AST *Parser::parseGSM()
{
    llvm::SmallVector<Expr *> exprs;
    while (!Tok.is(Token::eoi))
    {
        switch (Tok.getKind())
        {
        case Token::KW_int:
            Expr *declaration;
            declaration = parseDeclaration();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error1;
            }
            if (declaration)
                exprs.push_back(declaration);
            else
                goto _error1;
            break;

        case Token::ident:
            Expr *assign;
            assign = parseAssign();

            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error1;
            }
            if (assign)
                exprs.push_back(assign);
            else
                goto _error1;
            break;

        case Token::KW_if:
            Expr *ifelse;
            ifelse = parseIfElse();

            if (ifelse)
                exprs.push_back(ifelse);
            else
                goto _error1;
            break;

        case Token::KW_loopc:
            Expr *loop;
            loop = parseLoop();

            if (loop)
                exprs.push_back(loop);
            else
                goto _error1;
            break;

        default:
            goto _error1;
            break;
        }
        advance(); // TODO: watch this part
    }
    return new GSM(exprs);
_error1:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseDeclaration()
{
    Expr *E;
    int vars_count = 0;
    int exprs_count = 0;
    llvm::SmallVector<llvm::StringRef, 8> Vars;
    llvm::SmallVector<Expr *> Exprs;

    if (!Tok.is(Token::KW_int))
        goto _error2;

    advance();

    if (expect(Token::ident))
        goto _error2;

    Vars.push_back(Tok.getText());
    vars_count += 1;
    advance();

    while (Tok.is(Token::comma))
    {
        advance();

        if (expect(Token::ident))
            goto _error2;

        Vars.push_back(Tok.getText());
        vars_count += 1;
        advance();
    }

    if (Tok.is(Token::equal))
    {
        advance();
        E = parseExpression();
        if (E) {
            Exprs.push_back(E);
            exprs_count += 1;
        }
        else {
            goto _error2;
        }

        while (Tok.is(Token::comma))
        {
            advance();
            E = parseExpression();
            if (E) {
                Exprs.push_back(E);
                exprs_count += 1;
            } 
            else {
                goto _error2;
            }
        }
    }

    if (expect(Token::semicolon) || exprs_count > vars_count)
        goto _error2;

    return new Declaration(Vars, Exprs);
_error2: // TODO: Check this later in case of error :)
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseAssign()
{
    Factor *F;
    Expr *E;
    F = (Factor *)(parseFactor());
    Assignment::Operator Op;

    if (!Tok.is(Token::equal) || !Tok.is(Token::plus_equal) || !Tok.is(Token::mult_equal)
    || !Tok.is(Token::div_equal) || !Tok.is(Token::minus_equal) || !Tok.is(Token::mod_equal))
    {
        error();
        return nullptr;
    }

    if (Tok.is(Token::equal)) {
        Op = Assignment::Eq;
    }
    else if(Tok.is(Token::plus_equal)) {
        Op = Assignment::PlEq;
    }
    else if(Tok.is(Token::mult_equal)) {
        Op = Assignment::MulEq;
    }
    else if(Tok.is(Token::div_equal)) {
        Op = Assignment::DivEq;
    }
    else if(Tok.is(Token::minus_equal)) {
        Op = Assignment::MinEq;
    }
    else {
        Op = Assignment::ModEq;
    }

    advance();
    E = parseExpression();
    return new Assignment(Op, F, E);
}

Expr *Parser::parseIfElse()
{
    Expr *E;
    // Expr *A;
    Assignment *A;

    llvm::SmallVector<Expr *> expressions;
    llvm::SmallVector<llvm::SmallVector<Assignment *>> assignments;
    llvm::SmallVector<Assignment *> temp_assignments;
    bool hasElse = false; //NEW

    if (!Tok.is(Token::KW_if))
        goto _error3;

    advance();

    E = parseExpression();
    if(E)
        expressions.push_back(E);
    else
        goto _error3;

    if (!Tok.is(Token::colon))
        goto _error3;

    advance();

    if (!Tok.is(Token::KW_begin))
        goto _error3;

    advance();

    while (!Tok.is(Token::KW_end))
    {
        if (!Tok.is(Token::ident))
        {
            error();
            goto _error3;
        }

        else {
            A = dynamic_cast<Assignment *>(parseAssign());
            // A = parseAssign();
            if(A)
                temp_assignments.push_back(A);
            else
                goto _error3;
        }
    }

    assignments.push_back(temp_assignments);

    if (!Tok.is(Token::KW_end))
    {
        error();
        goto _error3;
    }

    advance();
    
    while (Tok.is(Token::KW_elif)) {
        advance();

        E = parseExpression();
        if(E)
            expressions.push_back(E);
        else
            goto _error3;

        if (!Tok.is(Token::colon))
            goto _error3;

        advance();

        if (!Tok.is(Token::KW_begin))
            goto _error3;

        advance();

        temp_assignments.clear();
        
        while (!Tok.is(Token::KW_end))
        {
            if (!Tok.is(Token::ident))
            {
                error();
                goto _error3;
            }

            else {
                A = dynamic_cast<Assignment *>(parseAssign());
                // A = parseAssign();
                if(A)
                    temp_assignments.push_back(A);
                else
                    goto _error3;
            }
        }

        assignments.push_back(temp_assignments);

        if (!Tok.is(Token::KW_end))
        {
            error();
            goto _error3;
        }

        advance();
    }
    
    if (Tok.is(Token::KW_else)) {
        advance();
        hasElse = true;//NEW

        if (!Tok.is(Token::colon))
            goto _error3;

        advance();

        if (!Tok.is(Token::KW_begin))
            goto _error3;

        advance();
        
        temp_assignments.clear();

        while (!Tok.is(Token::KW_end))
        {
            if (!Tok.is(Token::ident))
            {
                error();
                goto _error3;
            }

            else {
                A = dynamic_cast<Assignment *>(parseAssign());
                // A = parseAssign();
                if(A)
                    temp_assignments.push_back(A);
                else
                    goto _error3;
            }
        }

        assignments.push_back(temp_assignments);

        if (!Tok.is(Token::KW_end))
        {
            error();
            goto _error3;
        }
    }
    return new IfElse(expressions, assignments, hasElse);// NEW
_error3:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseLoop()
{
    Expr *E;
    // Expr *A;
    Assignment *A;
    llvm::SmallVector<Assignment *> assignments;

    if (!Tok.is(Token::KW_loopc))
        goto _error4;

    advance();

    E = parseExpression();

    if (!Tok.is(Token::colon))
        goto _error4;

    advance();

    if (!Tok.is(Token::KW_begin))
        goto _error4;

    advance();

    while (!Tok.is(Token::KW_end))
    {
        if (!Tok.is(Token::ident))
        {
            error();
            goto _error4;
        }

        else {
            A = dynamic_cast<Assignment *>(parseAssign());
            // A = parseAssign();
            if(A)
                assignments.push_back(A);
            else
                goto _error4;
        }
    }

    if (!Tok.is(Token::KW_end))
    {
        error();
        goto _error4;
    }

    return new Loop(E, assignments);
_error4:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseExpression()
{
    Expr *Left = parseDisjunction();

    while (Tok.is(Token::KW_logical_or))
    {
        BinaryOp::Operator Op = BinaryOp::Or;
        advance();
        Expr *Right = parseDisjunction();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseDisjunction()
{
    Expr *Left = parseConjunction();

    while (Tok.is(Token::KW_logical_and))
    {
        BinaryOp::Operator Op = BinaryOp::And;
        advance();
        Expr *Right = parseConjunction();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseConjunction()
{
    Expr *Left = parseEquality();
    
    while (Tok.isOneOf(Token::is_equal, Token::is_not_equal))
    {
        BinaryOp::Operator Op =
            Tok.is(Token::is_equal) ? BinaryOp::IsEq : BinaryOp::IsNEq;
        advance();
        Expr *Right = parseEquality();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseEquality()
{
    Expr *Left = parseSoftComparison();
    
    while (Tok.isOneOf(Token::soft_comp_greater, Token::soft_comp_lower))
    {
        BinaryOp::Operator Op =
            Tok.is(Token::soft_comp_greater) ? BinaryOp::GrEq : BinaryOp::LoEq;
        advance();
        Expr *Right = parseSoftComparison();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseSoftComparison()
{
    Expr *Left = parseHardComparison();
    
    while (Tok.isOneOf(Token::hard_comp_greater, Token::hard_comp_lower))
    {
        BinaryOp::Operator Op =
            Tok.is(Token::hard_comp_greater) ? BinaryOp::Gr : BinaryOp::Lo;
        advance();
        Expr *Right = parseHardComparison();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseHardComparison()
{
    Expr *Left = parsePlusMinus();
    
    while (Tok.isOneOf(Token::plus, Token::minus))
    {
        BinaryOp::Operator Op =
            Tok.is(Token::plus) ? BinaryOp::Plus : BinaryOp::Minus;
        advance();
        Expr *Right = parsePlusMinus();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parsePlusMinus()
{
    Expr *Left = parseTerm();
    BinaryOp::Operator Op;
    
    while (Tok.isOneOf(Token::star, Token::slash, Token::mod))
    {
        if (Tok.is(Token::star)) {
            Op = BinaryOp::Mul;
        }
        else if(Tok.is(Token::slash)) {
            Op = BinaryOp::Div;
        }
        else {
            Op = BinaryOp::Mod;
        }
        advance();
        Expr *Right = parseTerm();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseTerm()
{
    Expr *Left = parseFactor();

    while (Tok.is(Token::power))
    {
        BinaryOp::Operator Op = BinaryOp::Pow;
        advance();
        Expr *Right = parseFactor();
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;
}

Expr *Parser::parseFactor()
{
    Expr *Res = nullptr;
    
    switch (Tok.getKind())
    {
    case Token::number:
        Res = new Factor(Factor::Number, Tok.getText());
        advance();
        break;
    case Token::ident:
        Res = new Factor(Factor::Ident, Tok.getText());
        advance();
        break;
    case Token::l_paren:
        advance();
        Res = parseExpression();
        if (!consume(Token::r_paren))
            break;
    default: // error handling
        if (!Res)
            error();
        while (!Tok.isOneOf(Token::power, Token::star, Token::slash, 
                            Token::mod, Token::plus, Token::minus,
                            Token::hard_comp_greater, Token::hard_comp_lower, 
                            Token::soft_comp_greater, Token::soft_comp_lower,
                            Token::is_equal, Token::is_not_equal, Token::KW_logical_and,
                            Token::KW_logical_or, Token::r_paren, Token::colon,
                            Token::semicolon, Token::comma, Token::eoi))
            advance();
        break;
    }
    return Res;
}
