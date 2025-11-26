#include "lexer.c"

// Terminals
typedef struct {
    Token int_lit;
} NodeTermIntLit;

// Expressions
typedef enum {
    NODE_EXPR_ADD,
    NODE_EXPR_SUB,
    NODE_EXPR_MUL,
    NODE_EXPR_DIV,
    NODE_EXPR_INT_LIT
} NodeExprKind;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprAdd;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprSub;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprMul;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprDiv;

typedef struct {
    NodeExprKind kind;
    union {
        NodeExprAdd *add;
        NodeExprSub *sub;
        NodeExprMul *mul;
        NodeExprDiv *div;
        NodeTermIntLit *int_lit;
    } as;
} NodeExpr;

// Statements
typedef enum {
    NODE_STMT_EXIT
} NodeStmtKind;

typedef struct {
    NodeExpr *expr;
} NodeStmtExit;

typedef struct {
    NodeStmtKind kind;
    union {
        NodeStmtExit *exit_stmt;
    } as;
} NodeStmt;

typedef struct {
    NodeStmt **statements;
    int count;
} NodeProg;