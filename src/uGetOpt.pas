unit uGetOpt;

{$mode objfpc}{$H+}

interface

uses
  getopts;

type
  TOptionHandler = procedure (const opt: string; const OptArg: string);
  TOptions = array[1..10] of TOption;
  POptions = ^TOptions;

(*
   Process all arguments as defined in ShortOpts and LongOpts
   Arguments:
     ShortOpts  - short option string (see getopts)
     LongOpts   - pointer to the first element of an array of long options, terminated by an empty option record (see getopts)
     Handler    - pointer to a function called to handle each option

   Result:
     The index of the first option not processed (this correctly handles the -- delimiter)
*)
function HandleAllOptions(ShortOpts : String; LongOpts : POptions; Handler: TOptionHandler): integer;

implementation

function HandleAllOptions(ShortOpts : String; LongOpts : POptions; Handler: TOptionHandler): integer;
var
  optIndex: integer;
  opt: string;
begin
  optIndex:= 0;
  while True do begin
    opt:= GetLongOpts(ShortOpts, POption(LongOpts), optIndex);
    if opt = EndOfOptions then
      break;
    if opt = #0 then
      opt:= LongOpts^[optIndex].Name;
    Handler(opt, getopts.optArg);
  end;
  Result:= getopts.OptInd;
end;

end.

