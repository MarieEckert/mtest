{$ifndef dcc}
	{$mode delphi}
{$endif}

unit shared;

interface

uses
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

{$i tests.inc}

implementation

end.
