header { 
  import java.io.*;
  import java.util.Vector;
  import java.util.Hashtable;
 }

/**
 *  The Express-X grammar written as ANTLR productions.
 *
 *  ---> This is a degen grammar <---
 * 
 *  Peter Denno, pdenno@nist.gov
 *  When refreshing the parser rules: 
 *   1) replace "q  col  =  q" with COLEQ
 *   2) rel_op  (COLLTGT , COLEQ )
 *   3) view_decl (COLON)
 *
 */
class DegenXParser extends Parser;
options {
	exportVocab=DegenX;
    /*analyzerDebug=true;*/
    interactive=true;
    k=1;
}


abstract_supertype_declaration
             :  "ABSTRACT"  "SUPERTYPE"  (  subtype_constraint  )? 
             ;

actual_parameter_list
             :  LPAREN!  parameter  (  COMMA!  parameter  )*  RPAREN! 
             ;

add_like_op
             :  PLUS  
             |  MINUS  
             |  "OR"  
             |  "XOR" 
             ;

aggregate_initializer
             :  LBRACK!  (  element  (  COMMA!  element  )*  )?  RBRACK! 
             ;

aggregate_source
             :  simple_expression 
             ;

aggregate_type
             :  "AGGREGATE"  (  COLON!  IDENT  )?  "OF"  parameter_type 
             ;

aggregation_types
             :  array_type  
             |  bag_type  
             |  list_type  
             |  set_type 
             ;

algorithm_head
             :  (  declaration  )*  (  constant_decl  )?  (  local_decl  )? 
             ;

array_type
             :  "ARRAY"  bound_spec  "OF"  (  "OPTIONAL"  )?  (  "UNIQUE"  )?  base_type 
             ;

backward_path_qualifier
             :  LTMINUS!  (  IDENT  )?  path_condition 
             ;

bag_type
             :  "BAG"  (  bound_spec  )?  "OF"  base_type 
             ;

base_type
             :  aggregation_types  
             |  simple_types  
             |  named_types 
             ;

binary_type
             :  "BINARY"  (  width_spec  )? 
             ;

binding_header
             :  (  "PARTITION"  partition_id  SEMI!  )?  (  from_clause  )?  (  local_decl  )?  (  where_clause  )?  (  identified_by_clause  )?  (  ordered_by_clause  )? 
             ;

boolean_type
             :  "BOOLEAN" 
             ;

bound_1
             :  numeric_expression 
             ;

bound_2
             :  numeric_expression 
             ;

bound_spec
             :  LBRACK!  bound_1  COLON!  bound_2  RBRACK! 
             ;

built_in_constant
             :  "CONST_E"  
             |  "PI"  
             |  "SELF"  
             |  QUESTION 
             ;

built_in_function
             :  "ABS"  
             |  "ACOS"  
             |  "ASIN"  
             |  "ATAN"  
             |  "BLENGTH"  
             |  "COS"  
             |  "EXISTS"  
             |  "EXTENT"  
             |  "EXP"  
             |  "FORMAT"  
             |  "HIBOUND"  
             |  "HIINDEX"  
             |  "LENGTH"  
             |  "LOBOUND"  
             |  "LOINDEX"  
             |  "LOG"  
             |  "LOG2"  
             |  "LOG10"  
             |  "NVL"  
             |  "ODD"  
             |  "ROLESOF"  
             |  "SIN"  
             |  "SIZEOF"  
             |  "SQRT"  
             |  "TAN"  
             |  "TYPEOF"  
             |  "USEDIN"  
             |  "VALUE"  
             |  "VALUE_IN"  
             |  "VALUE_UNIQUE" 
             ;

built_in_procedure
             :  "INSERT"  
             |  "REMOVE" 
             ;

case_action
             :  case_label  (  COMMA!  case_label  )*  COLON!  stmt 
             ;

case_expr
             :  "CASE"  selector  "OF"  (  case_expr_action  )*  (  "OTHERWISE"  COLON!  expression  )?  "END_CASE" 
             ;

case_expr_action
             :  case_label  (  COMMA!  case_label  )*  COLON!  expression  SEMI! 
             ;

case_label
             :  expression 
             ;

case_stmt
             :  "CASE"  selector  "OF"  (  case_action  )*  (  "OTHERWISE"  COLON!  stmt  )?  "END_CASE"  SEMI! 
             ;

compound_stmt
             :  "BEGIN"  stmt  (  stmt  )*  "END"  SEMI! 
             ;

constant_body
             :  constant_id  COLON!  base_type  COLEQ  expression  SEMI! 
             ;

constant_decl
             :  "CONSTANT"  constant_body  (  constant_body  )*  "END_CONSTANT"  SEMI! 
             ;


constant_id
             :  IDENT 
             ;

declaration
             :  function_decl  
             |  procedure_decl 
             ;

dependent_map_decl
             :  "DEPENDENT_MAP"  map_id  "AS"  target_parameter  SEMI!  (  target_parameter  SEMI!  )*  (  map_subtype_of_clause  )?  dep_map_partition  (  dep_map_partition  )*  "END_DEPENDENT_MAP"  SEMI! 
             ;

dep_binding_decl
             :  dep_from_clause  (  where_clause  )?  (  ordered_by_clause  )? 
             ;

dep_from_clause
             :  "FROM"  dep_source_parameter  SEMI!  (  dep_source_parameter  SEMI!  )* 
             ;

dep_map_decl_body
             :  dep_binding_decl  map_project_clause 
             ;

dep_map_partition
             :  (  "PARTITION"  partition_id  COLON!  )?  dep_map_decl_body 
             ;

dep_source_parameter
             : IDENT  ( COMMA! IDENT )* COLON! (simple_types | ident_dot_ident ) 
             ;

domain_rule
             :  (  label  COLON!  )?  logical_expression 
             ;

element
             :  expression  (  COLON!  repetition  )? 
             ;

entity_id
             :  IDENT 
             ;

entity_instantiation_loop
             :  "FOR"  instantiation_loop_control  SEMI!  map_project_clause 
             ;

escape_stmt
             :  "ESCAPE"  SEMI! 
             ;

expression
             :  simple_expression  (  rel_op_extended  simple_expression  )? 
             ;

expression_or_wild
             :  expression  
             |  UNDERSCORE
             ;

extent_reference
             :  ident_dot_ident  
             ;

factor
             :  simple_factor  (  DOUBLESTAR  simple_factor  )? 
             ;

foreach_expr
             :  "EACH"  variable_id  "IN"  expression  (  where_clause  )?  "RETURN"  expression 
             ;

forloop_expr
             :  repeat_control  "RETURN"  expression 
             ;

formal_parameter
             :  IDENT  (  COMMA!  IDENT  )*  COLON!  parameter_type 
             ;

forward_path_qualifier
             :  "::"  IDENT  (  path_condition  )? 
             ;

for_expr
             :  "FOR"  ( foreach_expr | forloop_expr ) 
             ;

from_clause
             :  "FROM"  ( source_parameter  SEMI! )+
             ;


function_decl
             :  function_head (  algorithm_head  )?  (stmt)+  "END_FUNCTION"  SEMI! 
             ;

function_head
             :  "FUNCTION"  function_id  (  LPAREN!  formal_parameter  (  SEMI!  formal_parameter  )*  RPAREN!  )?  
                COLON!  parameter_type  SEMI! 
             ;

function_id
             :  IDENT 
             ;

generalized_types
             :  aggregate_type  
             |  general_aggregation_types  
             |  generic_type 
             ;

general_aggregation_types
             :  general_array_type  
             |  general_bag_type  
             |  general_list_type  
             |  general_set_type 
             ;

general_array_type
             :  "ARRAY"  (  bound_spec  )?  "OF"  (  "OPTIONAL"  )?  (  "UNIQUE"  )?  parameter_type 
             ;


general_bag_type
             :  "BAG"  (  bound_spec  )?  "OF"  parameter_type 
             ;

general_list_type
             :  "LIST"  (  bound_spec  )?  "OF"  (  "UNIQUE"  )?  parameter_type 
             ;



general_set_type
             :  "SET"  (  bound_spec  )?  "OF"  parameter_type 
             ;

generic_type
             :  "GENERIC"  (  COLON!  IDENT  )? 
             ;


identified_by_clause
             :  "IDENTIFIED_BY"  id_parameter  SEMI!  (  id_parameter  SEMI!  )* 
             ;

/* POD unfortunately, ID COLON! should be optional! */
id_parameter
             :   IDENT  COLON! ( expression )?
             ;


ident_dot_ident
             :   IDENT ( DOT! IDENT )?
             ; 

if_expr
             :  "IF"  logical_expression  "THEN"  expression  (  "ELSIF"  logical_expression  expression  )*  (  "ELSE"  expression  )?  "END_IF" 
             ;

if_stmt
             :  "IF"  logical_expression  "THEN"  ( stmt )+  (  "ELSE"  (stmt)+  )?  "END_IF"  SEMI! 
             ;

increment
             :  numeric_expression 
             ;

increment_control
             :  variable_id  COLEQ  bound_1  "TO"  bound_2  (  "BY"  increment  )? 
             ;

index
             :  numeric_expression 
             ;

index_1
             :  index 
             ;

index_2
             :  index 
             ;

index_qualifier
             :  LBRACK!  index_1  (  COLON!  index_2  )?  RBRACK! 
             ;

instantiation_foreach_control
             :  "EACH"  variable_id  "IN"  ident_dot_ident  "INDEXING"  variable_id  
                     (  variable_id  "IN"  ident_dot_ident  "INDEXING"  variable_id  )* 
             ;

instantiation_loop_control
             :  instantiation_foreach_control  
             |  repeat_control 
             ;

integer_type
             :  "INTEGER" 
             ;

interval
             :  LCURLY!  interval_low  interval_op  interval_item  interval_op  interval_high  RCURLY! 
             ;

interval_high
             :  simple_expression 
             ;

interval_item
             :  simple_expression 
             ;

interval_low
             :  simple_expression 
             ;

interval_op
             :  LT  
             |  LE 
             ;

label
             :  IDENT 
             ;

list_type
             :  "LIST"  (  bound_spec  )?  "OF"  (  "UNIQUE"  )?  base_type 
             ;

literal
             :  logical_literal  
             |  STRING_LITERAL 
             ;

local_decl
             :  "LOCAL"  (local_variable)+  "END_LOCAL"  SEMI! 
             ;

local_variable
             :  variable_id  (  COMMA!  variable_id  )*  COLON!  parameter_type  (  COLEQ  expression  )?  SEMI! 
             ;

logical_expression
             :  expression 
             ;

logical_literal
             :  "FALSE"  
             |  "TRUE"  
             |  "UNKNOWN" 
             ;

logical_type
             :  "LOGICAL" 
             ;

map_attribute_declaration
             : ( IDENT (index_qualifier)?  (BACKSLASH! IDENT)? ( DOT! IDENT)? )? (index_qualifier)?
               COLEQ  expression  SEMI! 
             ;


map_decl
             :  "MAP"  map_id  "AS"  (target_parameter  SEMI!)+  
                (   (map_subtype_of_clause  ( subtype_binding_header map_decl_body )+  )
                 |  (  ( binding_header map_decl_body )+ ))
               "END_MAP"  SEMI! 
             ;

map_decl_body
             :  (   ( entity_instantiation_loop )+  )  
             |  map_project_clause  
             |  (  "RETURN"  expression  SEMI!  ) 
             ;

map_id
             :  IDENT 
             ;

map_project_clause
             :  "SELECT"  ( map_attribute_declaration )+ 
             ;


map_subtype_of_clause
             :  "SUBTYPE"  "OF"  LPAREN!  ident_dot_ident  RPAREN!  SEMI! 
             ;

multiplication_like_op
             :  STAR  
             |  DIVSIGN  
             |  "DIV"  
             |  "MOD"  
             |  "AND"  
             |  DOUBLEBAR
             ;

named_types
             :  IDENT  
             ;

null_stmt
             :  SEMI! 
             ;

number_type
             :  "NUMBER" 
             ;

numeric_expression
             :  simple_expression 
             ;

one_of
             :  "ONEOF"  LPAREN!  supertype_expression  (  COMMA!  supertype_expression  )*  RPAREN! 
             ;

ordered_by_clause
             :  "ORDERED_BY"  ( expression  SEMI! )+
             ;

parameter
             :  expression 
             ;


parameter_type
             :  generalized_types  
             |  named_types  
             |  simple_types 
             ;

partition_id
             :  IDENT 
             ;


path_condition
             :  LCURLY!  extent_reference  (  BAR!  logical_expression  )?  RCURLY! 
             ;

path_qualifier
             :  forward_path_qualifier  
             |  backward_path_qualifier 
             ;


precision_spec
             :  numeric_expression 
             ;

procedure_decl
             :  procedure_head  (  algorithm_head  )?  (  stmt  )*  "END_PROCEDURE"  SEMI! 
             ;

procedure_head
             :  "PROCEDURE"  procedure_id  (  LPAREN!  (  "VAR"  )?  formal_parameter  (  SEMI!  (  "VAR"  )?  formal_parameter  )*  RPAREN!  )?  SEMI! 
             ;

procedure_id
             :  IDENT 
             ;


query_expression
             :  "QUERY"  LPAREN!  variable_id  LTSTAR!  aggregate_source  BAR!  logical_expression  RPAREN! 
             ;

real_type
             :  "REAL"  (  LPAREN!  precision_spec  RPAREN!  )? 
             ;

reference_clause
             :  "REFERENCE"  "FROM"  schema_ref_or_rename  (  LPAREN!  resource_or_rename  (  COMMA!  resource_or_rename  )*  RPAREN!  )?  (  "AS"  (  "SOURCE"  
             |  "TARGET"  )  )?  SEMI! 
             ;

rel_op
             :  LT  
             |  GT  
             |  LE  
             |  GE  
             |  LTGT  
             |  ASSIGN  
             |  ":<>:"
             |  ":=:"
             ;

rel_op_extended
             :  rel_op  
             |  "IN"  
             |  "LIKE" 
             ;

rename_id
             :  IDENT
             ;

repeat_control
             :  (  increment_control  )?  (  while_control  )?  (  until_control  )? 
             ;

repeat_stmt
             :  "REPEAT"  repeat_control  SEMI!  ( stmt )+  "END_REPEAT"  SEMI! 
             ;

repetition
             :  numeric_expression 
             ;

resource_or_rename
             :  resource_ref  (  "AS"  rename_id  )? 
             ;

resource_ref
             :  IDENT  
             ;

return_stmt
             :  "RETURN"  (  LPAREN!  expression  RPAREN!  )?  SEMI! 
             ;

rule_decl
             :  rule_head (  stmt  )*  where_clause  "END_RULE"  SEMI! 
             ;

rule_head
             :  "RULE"  rule_id  "FOR"  LPAREN!  IDENT  (  COMMA!  IDENT  )*  RPAREN!  SEMI! 
             ;

rule_id
             :  IDENT 
             ;


schema_map_body_element
             :  function_decl  
             |  procedure_decl  
             |  view_decl  
             |  map_decl  
             |  dependent_map_decl  
             |  rule_decl 
             ;

schema_map_body_element_list
             :   ( schema_map_body_element )+ 
             ;

schema_map_decl
             :  "SCHEMA_MAP"  IDENT  ( reference_clause )+  (  constant_decl  )?  schema_map_body_element_list  "END_SCHEMA_MAP"  SEMI! 
             ;

schema_ref_or_rename
             :    IDENT ( COLON!  IDENT )?
             ;

schema_view_body_element
             :  function_decl  
             |  procedure_decl  
             |  view_decl  
             |  rule_decl 
             ;

schema_view_body_element_list
             :  ( schema_view_body_element )+ 
             ;

schema_view_decl
             :  "SCHEMA_VIEW"  IDENT  ( reference_clause )*  ( constant_decl )?  schema_view_body_element_list  
                "END_SCHEMA_VIEW"  SEMI! 
             ;


selector
             :  expression 
             ;

set_type
             :  "SET"  (  bound_spec  )?  "OF"  base_type 
             ;

simple_expression
             :  term  (  add_like_op  term  )* 
             ;


/* First rule is general_attribute_qualifier */
/* Second rule is group_qualifier */
/* index_qualifier is [n] or [n:m] path_qualifier is <- ident */
qualifier
             :  DOT! IDENT  
             |  BACKSLASH!  IDENT 
             |  index_qualifier  
             |  path_qualifier 
             ;


simple_qualifier
             :  index_qualifier  
             |  path_qualifier 
             ;

/* first rule incorporates enumeration_reference, entity_constructor, unary_op ( expression ) | primary */
/* and primary itself subsumes qualifiable_factor */

/* simple_factor -> aggregate_initializer         */
/* qualifier -> index_qualifier                   */
/* POD I removed  aggregate_initializer....       */
/* schemaName.id(                                 */

/* 2001/3/12 problem is definitely with LBRACK    */
/*    aggregate_initializer ->  LBRACK!...        */
simple_factor
             : ( (unary_op)?  (    ( IDENT (factor_qualifier)* )
                                |  ( built_in_constant (factor_qualifier)* )
                                |  ( built_in_function (factor_qualifier)* )
                                |  literal )
             |  LPAREN expression RPAREN
             |  aggregate_initializer 
             |  interval  
             |  query_expression  
             |  case_expr
             |  for_expr
             |  if_expr
             ;


factor_qualifier
             : ( AT! IDENT )
             | ( DOT! IDENT)
             | ( BACKSLASH! IDENT )
             | index_qualifier
             | path_qualifier
             | LPAREN! Expression_or_wild (COMMA! Expression_or_wild)* RPAREN!
             ;

simple_types
             :  binary_type  
             |  boolean_type  
             |  integer_type  
             |  logical_type  
             |  number_type  
             |  real_type  
             |  string_type 
             ;

skip_stmt
             :  "SKIP"  SEMI! 
             ;

source_parameter
             :  IDENT COLON! extent_reference 
             ;


/* First block incorporates procedure_call_stmt and assignment_stmt */
stmt
             :  ( built_in_procedure actual_parameter_list ) | 
                ( IDENT ( (actual_parameter_list)? |  ( (qualifier)* COLEQ expression )) )
                SEMI!
             |  case_stmt  
             |  compound_stmt  
             |  escape_stmt  
             |  if_stmt  
             |  null_stmt  
             |  repeat_stmt  
             |  return_stmt  
             |  skip_stmt 
             ;


string_type
             :  "STRING"  (  width_spec  )? 
             ;

subsuper
             :  (  supertype_constraint  )?  (  subtype_declaration  )? 
             ;

subtype_binding_header
             :  (  "PARTITION"  (  partition_id  )?  SEMI!  )?  where_clause 
             ;

subtype_constraint
             :  "OF"  LPAREN!  supertype_expression  RPAREN! 
             ;

subtype_declaration
             :  "SUBTYPE"  "OF"  LPAREN!  IDENT  (  COMMA!  IDENT  )*  RPAREN! 
             ;

supertype_constraint
             :  abstract_supertype_declaration  
             |  supertype_rule 
             ;

supertype_expression
             :  supertype_factor  (  "ANDOR"  supertype_factor  )* 
             ;

supertype_factor
             :  supertype_term  (  "AND"  supertype_term  )* 
             ;

supertype_rule
             :  "SUPERTYPE"  (  subtype_constraint  )? 
             ;

supertype_term
             :  IDENT  
             |  one_of  
             |  LPAREN!  supertype_expression  RPAREN! 
             ;

syntax_x
             :  schema_map_decl  
             |  schema_view_decl 
             ;

target_entity_reference
             :  ident_dot_ident  (  AMPERSAND!  ident_dot_ident  )* 
             ;

target_parameter
             :  IDENT  (  COMMA!  IDENT  )*  COLON!  (  "AGGREGATE"  (  bound_spec  )?  "OF"  )?  target_entity_reference 
             ;

term
             :  factor  (  multiplication_like_op  factor  )* 
             ;


unary_op
             :  PLUS  
             |  MINUS  
             |  "NOT" 
             ;

until_control
             :  "UNTIL"  logical_expression 
             ;

variable_id
             :  IDENT 
             ;

view_attribute_decl
             :  IDENT COLON! ("OPTIONAL")? (aggregation_types | simple_types | (IDENT (DOT! IDENT))) COLEQ expression SEMI! 
             ;


view_attr_decl_stmt_list
             :  view_attribute_decl  (  view_attribute_decl  )* 
             ;


view_decl
             :  "VIEW"  view_id  (  COLON!  base_type  )?  subsuper  SEMI!  (  subtype_binding_header  view_project_clause  (  subtype_binding_header  view_project_clause  )*  )  
             |  (  binding_header  view_project_clause  (  binding_header  view_project_clause  )*  )  "END_VIEW"  SEMI! 
             ;

view_id
             :  IDENT 
             ;

view_project_clause
             :  (  "SELECT"  view_attr_decl_stmt_list  )  
             |  (  "RETURN"  expression  ) 
             ;


where_clause
             :  "WHERE"  domain_rule  SEMI!  (  domain_rule  SEMI!  )* 
             ;

while_control
             :  "WHILE"  logical_expression 
             ;

width
             :  numeric_expression 
             ;

width_spec
             :  LPAREN!  width  RPAREN!  (  "FIXED"  )? 
             ;
/************************ These are from postlogue.g *******************************/
/* ExpressX LEXICAL RULES  */
class DegenXLexer extends Lexer;
options {
	exportVocab=DegenX;
	k=4;
}

SEMI
options {
  paraphrase = ";";
}
	:	';'
	;

QUESTION
options {
  paraphrase = "?";
}
	:	'?'
	;

LPAREN
options {
  paraphrase = "(";
}
	:	'('
	;

RPAREN
options {
  paraphrase = ")";
}
	:	')'
	;

LBRACK
options {
  paraphrase = "[";
}
	:	'['
	;

RBRACK
options {
  paraphrase = "]";
}
	:	']'
	;

LCURLY
options {
  paraphrase = "{";
}
	:	'{'
	;

RCURLY
options {
  paraphrase = "}";
}
	:	'}'
	;

BAR
options {
  paraphrase = "|";
}
	:	'|'
	;

AMPERSAND
options {
  paraphrase = "&";
}
	:	'&'
	;

COLON
options {
  paraphrase = ":";
}
	:	':'
	;

COMMA
options {
  paraphrase = ",";
}
	:	','
	;

DOT
options {
  paraphrase = ".";
}
	:	'.'
	;

ASSIGN
options {
  paraphrase = "=";
}
	:	'='
	;

LT
options {
  paraphrase = "<";
}
	:	'<'
	;

LE
options {
  paraphrase = "<=";
}
	:	'<'
		'='
	;

GT
options {
  paraphrase = ">";
}
	:	'>'
	;

GE
options {
  paraphrase = ">=";
}
	:	'>'
		'='
	;

DIVSIGN
options {
  paraphrase = "/";
}
	:	'/'
	;

PLUS
options {
  paraphrase = "+";
}
	:	'+'
	;

MINUS
options {
  paraphrase = "-";
}
	:	'-'
	;

STAR
options {
  paraphrase = "*";
}
	:	'*'
	;

AT
options {
  paraphrase = "@";
}
	:	'@'
	;

LTSTAR
options {
  paraphrase = "less than star";
}
        : '<' 
          '*'
        ;

LTGT
options {
  paraphrase = "less-than/greater-than thing";
}
        : '<' 
          '>'
        ;

DOUBLESTAR
options {
  paraphrase = "double star";
}
        : '*' 
          '*'
        ;

DOUBLEBAR
options {
  paraphrase = "double bar";
}
        : '|' 
          '|'
        ;

LTMINUS
options {
  paraphrase = "<-";
}
        : '<' 
          '-'
        ;

DOUBLECOLON
options {
  paraphrase = "::";
}
        : ':' 
          ':'
        ;

UNDERSCORE
options {
  paraphrase = "_";
}
        : '_' 
        ;

STRING_LITERAL 	
options {
  paraphrase = "a string literal";
}
	:
	'\'' 
	(~'\'')* 
	'\''
	;

protected
DIGIT
options {
  paraphrase = "a digit";
}
	:	'0'..'9'
	;

INT	
options {
  paraphrase = "an integer value";
}
	:    (DIGIT)+                  
	;

FLOAT
options {
  paraphrase = "an floating point value";
}

	:    '.' (DIGIT)+ (('e' | 'E') ('+' | '-')? (DIGIT)+)?
     	;

IDENT
options {
  testLiterals = true;
  paraphrase = "an identifer";
}

	:	('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
	;


