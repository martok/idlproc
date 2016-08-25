%{
(*
Vorspann
 ****************************************************************************)

unit parser;

interface

uses
  lexlib, yacclib;

             
function yyparse : Integer;

implementation

var
  st_in_attrib_list: boolean = false;

procedure yyerror ( msg : String );
begin
  writeln('at line ',yylineno,':',yycolno,' "',yytext,'":');
  WriteLn('  ',msg);
end(*yyerrmsg*);


%}

%token ILLEGAL
%token COLON SEMICOLON COMMA EQUAL
%token LKLAMMER RKLAMMER LECKKLAMMER RECKKLAMMER LGKLAMMER RGKLAMMER
%token _LIBRARY _INTERFACE _MODULE _TYPEDEF _STRUCT _UNION _ENUM _CONST
%token _IN _OUT _INOUT
%token ID NUMBER CSTRING IID   


%right _ASSIGN
%right R_AND

%left EQUAL UNEQUAL GT LT GTE LTE
%left QUESTIONMARK COLON
%%

idlfile
    : declaration_list               {  }
    ;

declaration_list
    : /* empty */                    {  }
    | declaration_list declaration   {  }
    ;

declaration
    : extension                      {  }
    | include                        {  }
    | declaration_body
    ;

attributes_maybe
    : /* empty */
    | LECKKLAMMER attribute_list RECKKLAMMER  { st_in_attrib_list:= false; }
    ;

attribute_list
    : attribute_list COMMA attribute          {  }
    | attribute                               {  }
    ;

attribute
    : attribute_tag ID LKLAMMER immediate RKLAMMER          {  }
    | attribute_tag ID                                      {  }
    ;

attribute_tag
    :                                         { st_in_attrib_list:= true; }
    ;

declaration_body
    : library                                 {  }
    | type_declaration                        {  }
    ;

library
    : attributes_maybe _LIBRARY ID LGKLAMMER type_declaration_list RGKLAMMER SEMICOLON  {  }
    ;

type_declaration_list
    : /* Empty */                             {  }
    | type_declaration_list type_declaration  {  }
    ;

type_declaration
    : interface                               {  }
    | module                                  {  }
    | typedef                                 {  }  
    | struct                                  {  }
    | union                                   {  }
    | enum                                    {  }
    ;

interface
    : attributes_maybe _INTERFACE ID interface_base LGKLAMMER interface_member_list RGKLAMMER SEMICOLON {  }
    ;

interface
    : attributes_maybe _MODULE ID LGKLAMMER module_member_list RGKLAMMER SEMICOLON {  }
    ;

interface_base
    : /* empty */                             {  }
    | COLON interfaceparents                  {  }
    ;

interfaceparents       
    : interfaceparents COMMA ID               {  }
    | ID                                      {  }
    ;

interface_member_list
    : interface_member_list interface_member  {  }
    | interface_member                        {  }
    ;

interface_member
    : const                                   {  }
    | property                                {  }
    | method                                  {  }
    ;

module_member_list
    : module_member_list module_member        {  }
    | module_member                           {  }
    ;

module_member
    : const                                   {  }
    | method                                  {  }
    ;

const
    : attributes_maybe _CONST typespec ID EQUAL immediate SEMICOLON          {  }
    ;
        
property
    : attributes_maybe ATTRIBUTE typespec ID SEMICOLON                       {  }
    ;

method
    : attributes_maybe typespec ID LKLAMMER paramlist RKLAMMER SEMICOLON     {  }
    ;

paramlist
    : paramlist COMMA param                   {  }
    | param                                   {  }
    |                                         {  }
    ;

param
    : attributes_maybe inoutspec typespec ID  {  }
    ;

/* TODO: configure matches */
inoutspec
    : _IN                                     {  }
    | _OUT                                    {  }
    | _INOUT                                  {  }
    | /* MSIDL: as attrib! */                 {  }
    ;

/* TODO: pointer etc*/
typespec
    : typespec ID                             {  }
    | ID                                      {  }
    ;

immediate
    : IID                                     {  }
    | NUMBER                                  { writeln(yytext); }
    | CSTRING                                 {  }
    ;

%%

{$I lexer.inc}

end.

