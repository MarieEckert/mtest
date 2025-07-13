{$ifndef dcc}
	{$mode delphi}
{$endif}
program mtest;

uses
	display,
	shared,
	Types;

var
	targets: TStringDynArray = ['x86_64', 'riscv', 'aarch64', 'powerpc'];
	tests: TTestSet = [
		(name: 'a'; status: [RunPassed, BuildFailed, BuildFailed, Ignored]),
		(name: 'b'; status: [RunFailed, Ignored, Ignored, BuildPassed]),
		(name: 'c'; status: [BuildFailed, BuildPassed, Ignored, Ignored]),
		(name: 'd'; status: [BuildPassed, RunPassed, RunFailed, RunFailed]),
		(name: 'd'; status: [Ignored, Ignored, Ignored, Ignored]),
		(name: 'e'; status: [RunPassed, RunPassed, RunPassed, RunPassed])
	];

	padding: Integer;
	test: TTestResults;
begin
	padding := 0;

	for test in tests do
		if Length(test.name) > padding then
			padding := Length(test.name);

	WriteLn;
	TargetHeader(targets, padding + 6);
	ResultRow(tests);
	Summary(targets, tests, padding);
end.
