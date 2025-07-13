{$ifndef dcc}
	{$mode delphi}
{$endif}

unit display;

interface

uses
	shared,
	Types;

procedure TargetHeader(const padding: Integer);

procedure ResultRow(constref tests: TResultsSet);

procedure Summary(
	constref tests: TResultsSet;
	const padding: Integer
);

{$include ansicodes.inc}

implementation

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

procedure ResultRow(constref tests: TResultsSet);
var
	test: TTestResults;
	status: TStatus;
begin
	for test in tests do
	begin
		Write('    ', test.name, ' => ');

		for status in test.status do
			ShowStatus(status);

		WriteLn;
	end;
end;

function CountStatus(const countee: TStatus; constref tests: TResultsSet): Integer;
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

end.
