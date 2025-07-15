{$ifndef dcc}
	{$mode delphi}
{$endif}
program mtest;

uses
	display,
	shared,
	runner,
	Types;

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
