program test;

begin
	WriteLn('Hello from test');
	Randomize;
	Halt(Random(101) <= 50);
end.
