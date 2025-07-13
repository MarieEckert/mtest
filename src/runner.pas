{$ifndef dcc}
	{$mode delphi}
{$endif}

unit runner;

interface

uses
	shared,
	SysUtils,
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
var
	passedArgs: TStringDynArray;
	exitCode: Integer;
begin
	result.bin := source + '.test';
	exitCode := ExecuteProcess(FPC_BIN, [source, '-o' + result.bin] + args);

	result.success := exitCode = 0;
end;

function Run(const bin: String; constref args: TStringDynArray): Integer;
begin
	result := ExecuteProcess(bin, args);
end;

end.
