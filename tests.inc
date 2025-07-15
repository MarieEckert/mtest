type
	TTarget = (x86_64, powerpc, aarch64, riscv);

	{ DO NOT MODIFY THIS RECORD }
	TTestArgs = record
		compileArgs: TStringDynArray;
		run: Boolean;
		args: TStringDynArray;
	end;

	TTest = record
		name: String;
		source: String;
		targets: array [TTarget] of TTestArgs;
		ignoredTargets: set of TTarget;
	end;

	TTestSet = array of TTest;

const
	FPC_BIN = '/usr/bin/fpc';

	tests: TTestSet = [
		(
			name: 'basic';
			source: 'tests/test.pas';
			targets: (
				( { x86_64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { powerpc }
					compileArgs: [];
					run: False;
					args: [];
				),
				( { aarch64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { riscv }
					compileArgs: [];
					run: True;
					args: [];
				)
			);
			ignoredTargets: [powerpc]
		),
		(
			name: 'compile';
			source: 'tests/test.pas';
			targets: (
				( { x86_64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { powerpc }
					compileArgs: [];
					run: False;
					args: [];
				),
				( { aarch64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { riscv }
					compileArgs: [];
					run: True;
					args: [];
				)
			);
			ignoredTargets: []
		),
		(
			name: 'rtl';
			source: 'tests/test.pas';
			targets: (
				( { x86_64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { powerpc }
					compileArgs: [];
					run: False;
					args: [];
				),
				( { aarch64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { riscv }
					compileArgs: [];
					run: True;
					args: [];
				)
			);
			ignoredTargets: []
		),
		(
			name: 'jens';
			source: 'tests/test.pas';
			targets: (
				( { x86_64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { powerpc }
					compileArgs: [];
					run: False;
					args: [];
				),
				( { aarch64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { riscv }
					compileArgs: [];
					run: True;
					args: [];
				)
			);
			ignoredTargets: [aarch64]
		),
		(
			name: '???';
			source: 'tests/test.pas';
			targets: (
				( { x86_64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { powerpc }
					compileArgs: [];
					run: False;
					args: [];
				),
				( { aarch64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { riscv }
					compileArgs: [];
					run: True;
					args: [];
				)
			);
			ignoredTargets: [aarch64]
		),
		(
			name: ':3c';
			source: 'tests/test.pas';
			targets: (
				( { x86_64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { powerpc }
					compileArgs: [];
					run: False;
					args: [];
				),
				( { aarch64 }
					compileArgs: [];
					run: True;
					args: [];
				),
				( { riscv }
					compileArgs: [];
					run: True;
					args: [];
				)
			);
			ignoredTargets: [aarch64, riscv]
		)
	];
