%token <string> CONSTRAINT
%token <int> INT
%token FALSE TRUE AND EOF OP CL EXP
%token TELL PARALLEL NEXT STAR BANG BOP BCL UNLESS COLON WHEN CHOICE PLUS

%left AND PARALLEL

%{
  open Ntcc 
  open Constraint
  open Utils
%}

%start <Ntcc.ntcc_program> main

%%

main:
  | p = ntcc_expr EOF { Some p }
  | EOF       { Empty }

constr_expr:
  | CONSTRAINT { Atom_C ($1) }
  | c1 = constr_expr AND c2 = constr_expr { And_C (c1, c2) }
  | FALSE { False_C }
  | TRUE  { True_C }
  | OP c = constr_expr CL   { c }

ntcc_expr:
  | TELL OP c = constr_expr CL { Tell (c) }
  | p1 = ntcc_expr PARALLEL p2 = ntcc_expr { Parallel (p1,p2) }
  | NEXT OP p =  ntcc_expr CL { Next (p) }
  | NEXT i = INT OP p = ntcc_expr CL { unfold_next p i }
  | STAR OP p = ntcc_expr CL { Star (p) }
  | BANG OP p = ntcc_expr CL { Bang (p) }
  | UNLESS BOP c = constr_expr COLON p = ntcc_expr BCL { Unless (c,p) }
  | w = when_field  { Choice [w] }
  | CHOICE OP l = choice CL { Choice l }


when_field:
  WHEN BOP c = constr_expr COLON p = ntcc_expr BCL  { (c,p) }

choice:
  c = separated_list(PLUS, when_field) { c }
