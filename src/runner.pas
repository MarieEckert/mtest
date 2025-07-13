{$ifndef dcc}
	{$mode delphi}
{$endif}

unit runner;

interface

uses
	shared,
	Types;

type
	TCompileResult = record
		success: Boolean;
		bin: String;
	end;


function Compile(
	const source: String;
	constref args: TStringDynArray
): TCompileResult;

function Run(const bin: String; constref args: TStringDynArray): Integer;

implementation

function Compile(
	const source: String;
	constref args: TStringDynArray
): TCompileResult;
begin
	result.success := True;
	result.bin := 'tests/' + source + '.test';
end;

function Run(const bin: String; constref args: TStringDynArray): Integer;
begin
	result := 0;
end;

end.
