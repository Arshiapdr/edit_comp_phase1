#ifndef PARSER_H
#define PARSER_H

#include "AST.h"
#include "Lexer.h"
#include "llvm/Support/raw_ostream.h"

class Parser
{
    Lexer &Lex;    // retrieve the next token from the input
    Token Tok;     // stores the next token
    bool HasError; // indicates if an error was detected

    void error()
    {
        llvm::errs() << "Unexpected: " << Tok.getText() << "\n";
        HasError = true;
    }

    // retrieves the next token from the lexer.expect()
    // tests whether the look-ahead is of the expected kind
    void advance() { Lex.next(Tok); }

    // Function to check the next token without advancing
    bool Parser::peek(Token::Kind expectedKind) {
        LexerState savedState = Tok.getSavedState(); // Save the current lexer state

        advance(); // Advance to the next token
        bool isExpected = Tok.is(expectedKind);

        // Restore the lexer state to its original state
        Tok.restoreState(savedState);

        return isExpected;
    }

    bool expect(Token::TokenKind Kind)
    {
        if (Tok.getKind() != Kind)
        {
            error();
            return true;
        }
        return false;
    }

    // retrieves the next token if the look-ahead is of the expected kind
    bool consume(Token::TokenKind Kind)
    {
        if (expect(Kind))
            return true;
        advance();
        return false;
    }

    AST *parseGSM();
    Expr *parseDeclaration();
    Assignment *parseAssign();
    Expr *parseIfElse();
    Expr *parseLoop();
    Expr *parseExpression();
    Expr *parseDisjunction();
    Expr *parseConjunction();
    Expr *parseEquality();
    Expr *parseSoftComparison();
    Expr *parseHardComparison();
    Expr *parsePlusMinus();
    Expr *parseTerm();
    Expr *parseFactor();

public:
    // initializes all members and retrieves the first token
    Parser(Lexer &Lex) : Lex(Lex), HasError(false)
    {
        advance();
    }

    // get the value of error flag
    bool hasError() { return HasError; }

    AST *parse();
};

#endif
