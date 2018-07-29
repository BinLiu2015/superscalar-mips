library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
	port(clk, reset: in STD_LOGIC;
		writedata, dataadr: buffer STD_LOGIC_VECTOR(31 downto 0);
		memwrite          : buffer STD_LOGIC);
end;

architecture test of top is
	component mips 
		port(clk, reset: in STD_LOGIC;
			pc               : in STD_LOGIC_VECTOR(31 downto 0);
			instr            : in  STD_LOGIC_VECTOR(31 downto 0);
			memwrite         : out STD_LOGIC;
			aluout, writedata: out STD_LOGIC_VECTOR(31 downto 0);
			readdata         : in  STD_LOGIC_VECTOR(31 downto 0);
			pcnext           : out  STD_LOGIC_VECTOR(31 downto 0));
	end component;
	-- component hazardunit 
	-- 	port(clk, reset: in STD_LOGIC;
	-- 		pc               : in STD_LOGIC_VECTOR(31 downto 0);
	-- 		instr            : in  STD_LOGIC_VECTOR(31 downto 0);
	-- 		memwrite         : out STD_LOGIC;
	-- 		aluout, writedata: out STD_LOGIC_VECTOR(31 downto 0);
	-- 		readdata         : in  STD_LOGIC_VECTOR(31 downto 0);
	-- 		pcnext           : out  STD_LOGIC_VECTOR(31 downto 0));
	-- end component;
	component flopr generic(width :    integer);
		port(clk, reset               : in STD_LOGIC;
		d : in  STD_LOGIC_VECTOR(width-1 downto 0);
		q : out STD_LOGIC_VECTOR(width-1 downto 0));
  	end component;
	component imem
		port(a: in STD_LOGIC_VECTOR(5 downto 0);
			rd: out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component dmem
		port(clk, we: in STD_LOGIC;
			a, wd: in  STD_LOGIC_VECTOR(31 downto 0);
			rd   : out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	signal pc, pcnext, instr, 
	readdata: STD_LOGIC_VECTOR(31 downto 0);
begin
	-- instantiate processor and memories
		mips1: mips port map(clk, reset, pc, instr, memwrite, dataadr, 
			writedata, readdata, pcnext);
		imem1: imem port map(pc(7 downto 2), instr);
		dmem1: dmem port map(clk, memwrite, dataadr, writedata, readdata);
		pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc); 

end;