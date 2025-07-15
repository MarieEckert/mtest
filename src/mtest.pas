{$ifndef dcc}
	{$mode delphi}
{$endif}
program mtest;

uses
	SysUtils,
	Types;

type
	TStatus = (
		RunPassed,
		RunFailed,
		BuildFailed,
		BuildPassed,
		Ignored
	);

	TStatusDynArray = array of TStatus;

	TTestResults = record
		name: String;
		status: TStatusDynArray;
	end;

	TResultsSet = array of TTestResults;

{$include tests.inc}

{******************************************************************************}
{ Display Logic                                                                }
{******************************************************************************}

{$include ansicodes.inc}

procedure WriteIndent(const amount: Integer; constref text: String);
var
	ix, six: Integer;
begin
	for ix := 0 to amount do
		Write(text);
end;

procedure TargetHeader(const padding: Integer);
var
	ix: Integer;
	target: TTarget;
begin
	ix := 0;

	for target in TTarget do
	begin
		WriteIndent(padding, ' ');
		WriteIndent(ix - 1, ' |');

		WriteLn(' ', target);

		Inc(ix);
	end;
end;

procedure ShowStatus(const status: TStatus);
begin
	case status of
		RunPassed: Write(STYLE_BOLD, COLOR_FORE_GREEN, 'P ', STYLE_RESET);
		RunFailed: Write(STYLE_BOLD, COLOR_FORE_RED, 'F ', STYLE_RESET);
		BuildFailed: Write(STYLE_BOLD, COLOR_FORE_YELLOW, 'C ', STYLE_RESET);
		BuildPassed: Write(STYLE_BOLD, COLOR_FORE_BLUE, 'B ', STYLE_RESET);
		Ignored: Write(STYLE_BOLD, COLOR_FORE_GRAY, '/ ', STYLE_RESET);
	end;
end;

procedure ResultRow(constref tests: TResultsSet; const padding: Integer);
var
	test: TTestResults;
	status: TStatus;
begin
	for test in tests do
	begin
		Write('    ', test.name, StringOfChar(' ', padding - Length(test.name)),
			  ' => ');

		for status in test.status do
			ShowStatus(status);

		WriteLn;
	end;
end;

function CountStatus(
	const countee: TStatus;
	constref tests: TResultsSet
): Integer;
var
	test: TTestResults;
	status: TStatus;
begin
	Result := 0;

	for test in tests do
		for status in test.status do
			if status = countee then
				Inc(Result);
end;

procedure Summary(
	constref tests: TResultsSet;
	const padding: Integer
	);
var
	ix, lines: Integer;
	lastTarget: String;

	procedure StatusLine(
			const status: TStatus;
			const color: String;
			constref tests: TResultsSet
			);
	begin
		WriteLn('    -> ', CountStatus(status, tests), ' × ',
				color, status, STYLE_RESET);
	end;

begin
	WriteStr(lastTarget, High(TTarget));

	lines := (
		  padding
		  + 6
		  + Length(tests[HIGH(tests)].status) * 2
		  + Length(lastTarget)
		  );

	WriteLn;
	Write('├');
	for ix := 0 to lines do
		Write('─');

	WriteLn('┤');
	WriteLn;
	WriteLn(STYLE_BOLD, '  SUMMARY:', STYLE_RESET);
	StatusLine(RunPassed, COLOR_FORE_GREEN, tests);
	StatusLine(BuildPassed, COLOR_FORE_BLUE, tests);
	StatusLine(RunFailed, COLOR_FORE_RED, tests);
	StatusLine(BuildFailed, COLOR_FORE_YELLOW, tests);
	StatusLine(Ignored, COLOR_FORE_GRAY, tests);
	WriteLn;
end;

{******************************************************************************}
{ Test Runner Code                                                             }
{******************************************************************************}

type
	TCompileResult = record
		success: Boolean;
		bin: String;
	end;

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

function RunTest(constref test: TTest): TTestResults;
var
	target: TTarget;
	compileResult: TCompileResult;
	ix, runResult: Integer;
begin
	ix := 0;
	result.name := test.name;

	SetLength(result.status, Ord(High(TTarget)) + 1);

	WriteLn(stderr, 'running test ', test.name);

	for target in TTarget do
	begin
		WriteLn(stderr, '  target: ', target);
		if target in test.ignoredTargets then
		begin
			WriteLn(stderr, '    -> ignored');
			result.status[ix] := Ignored;
			Inc(ix);
			continue;
		end;

		WriteLn(stderr, '    -> compiling');
		compileResult := Compile(test.source, test.targets[target].compileArgs);

		if not compileResult.success then
		begin
			WriteLn(stderr, '    [BUILD FAILURE]');
			result.status[ix] := BuildFailed;
			Inc(ix);
			continue;
		end;

		result.status[ix] := BuildPassed;
		if not test.targets[target].run then
		begin
			Inc(ix);
			continue;
		end;

		Writeln(stderr, '    -> running');
		runResult := Run(compileResult.bin, test.targets[target].args);

		if runResult = 0 then
			result.status[ix] := RunPassed
		else begin
			result.status[ix] := RunFailed;
			WriteLn(stderr, '    [RUN FAILURE]');
		end;

		Inc(ix);
	end;
end;

var
	results: TResultsSet;

	ix, padding: Integer;

	test: TTest;
	res: TTestResults;
begin
	ix := 0;
	SetLength(results, Length(tests));

	for test in tests do
	begin
		results[ix] := RunTest(test);
		Inc(ix);
	end;

	padding := 0;

	for res in results do
		if Length(res.name) > padding then
			padding := Length(res.name);

	WriteLn;
	TargetHeader(padding + 6);
	ResultRow(results, padding);
	Summary(results, padding);
end.
