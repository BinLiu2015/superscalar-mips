library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
	port(clk, reset: in STD_LOGIC;
		writedata1, dataadr1: buffer STD_LOGIC_VECTOR(31 downto 0);
		writedata2, dataadr2: buffer STD_LOGIC_VECTOR(31 downto 0);
		writedata3, dataadr3: buffer STD_LOGIC_VECTOR(31 downto 0);
		memwrite1          : buffer STD_LOGIC;
		memwrite2          : buffer STD_LOGIC;
		memwrite3          : buffer STD_LOGIC);
end;

architecture test of top is
	component mips 
		port(clk, reset: in STD_LOGIC;
			pc               : in STD_LOGIC_VECTOR(31 downto 0);
			instr            : in  STD_LOGIC_VECTOR(31 downto 0);
			stall			 : in  STD_LOGIC;			
			memwrite         : out STD_LOGIC;
			aluout			 : out STD_LOGIC_VECTOR(31 downto 0);
			writedata		 : in  STD_LOGIC_VECTOR(31 downto 0);
			readdata         : in  STD_LOGIC_VECTOR(31 downto 0);
			pcnext           : out STD_LOGIC_VECTOR(31 downto 0);
			regwrite         : out STD_LOGIC;
			writereg         : out STD_LOGIC_VECTOR(4 downto 0);
			result	         : out STD_LOGIC_VECTOR(31 downto 0);
			srca             : in  STD_LOGIC_VECTOR(31 downto 0);
			jump             : buffer  STD_LOGIC;
			pcsrc            : buffer  STD_LOGIC);
	end component;
	component hazardunit 
		port(clk, reset: in STD_LOGIC;
			pc               : out  STD_LOGIC_VECTOR(31 downto 0);
			pc1              : in   STD_LOGIC_VECTOR(31 downto 0);
			instr1           : in   STD_LOGIC_VECTOR(31 downto 0);
			jump1            : in   STD_LOGIC;
			pcsrc1           : in   STD_LOGIC;
			writereg1        : in   STD_LOGIC;
			pcaddr1          : out  STD_LOGIC_VECTOR(31 downto 0);
			pc2              : in   STD_LOGIC_VECTOR(31 downto 0);
			instr2           : in   STD_LOGIC_VECTOR(31 downto 0);
			jump2            : in   STD_LOGIC;
			pcsrc2           : in   STD_LOGIC;
			writereg2        : in   STD_LOGIC;
			pcaddr2          : out  STD_LOGIC_VECTOR(31 downto 0);
			stall2           : buffer  STD_LOGIC;
			pc3              : in   STD_LOGIC_VECTOR(31 downto 0);
			instr3           : in   STD_LOGIC_VECTOR(31 downto 0);
			jump3            : in   STD_LOGIC;
			pcsrc3           : in   STD_LOGIC;
			writereg3        : in   STD_LOGIC;
			pcaddr3          : out  STD_LOGIC_VECTOR(31 downto 0);
			stall3           : out  STD_LOGIC);
	end component;
	component flopr generic(width :    integer);
		port(clk, reset               : in STD_LOGIC;
		d : in  STD_LOGIC_VECTOR(width-1 downto 0);
		q : out STD_LOGIC_VECTOR(width-1 downto 0));
  	end component;
	component imem
		port(a: in STD_LOGIC_VECTOR(5 downto 0);
			rd1: out STD_LOGIC_VECTOR(31 downto 0);
			rd2: out STD_LOGIC_VECTOR(31 downto 0);
			rd3: out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component dmem
		port(clk	 : in  STD_LOGIC; 
			 we1	 : in  STD_LOGIC;
			 a1		 : in  STD_LOGIC_VECTOR(31 downto 0);
			 wd1	 : in  STD_LOGIC_VECTOR(31 downto 0);
			 rd1 	 : out STD_LOGIC_VECTOR(31 downto 0);
			 we2	 : in  STD_LOGIC;
			 a2	 	 : in  STD_LOGIC_VECTOR(31 downto 0);
			 wd2	 : in  STD_LOGIC_VECTOR(31 downto 0);
			 rd2  	 : out STD_LOGIC_VECTOR(31 downto 0);
			 we3	 : in  STD_LOGIC;
			 a3		 : in  STD_LOGIC_VECTOR(31 downto 0);
			 wd3	 : in  STD_LOGIC_VECTOR(31 downto 0);
			 rd3 	 : out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component regfile
		port(clk : in STD_LOGIC;
			 writeEn1     	   : in  STD_LOGIC;
			 ra1A, ra1B, wa1   : in  STD_LOGIC_VECTOR(4 downto 0);
			 writedata1   	   : in  STD_LOGIC_VECTOR(31 downto 0);
			 read1A, read1B    : out STD_LOGIC_VECTOR(31 downto 0);
			 writeEn2     	   : in  STD_LOGIC;
			 ra2A, ra2B, wa2   : in  STD_LOGIC_VECTOR(4 downto 0);
			 writedata2   	   : in  STD_LOGIC_VECTOR(31 downto 0);
			 read2A, read2B    : out STD_LOGIC_VECTOR(31 downto 0);
			 writeEn3     	   : in  STD_LOGIC;
			 ra3A, ra3B, wa3   : in  STD_LOGIC_VECTOR(4 downto 0);
			 writedata3   	   : in  STD_LOGIC_VECTOR(31 downto 0);
			 read3A, read3B    : out STD_LOGIC_VECTOR(31 downto 0));  
  	end component;
	signal pc: STD_LOGIC_VECTOR(31 downto 0);

	signal regwrite1, pcsrc1, jump1: STD_LOGIC;
	signal instr1, pcaddr1, readdata1, pc1, result1, srca1: STD_LOGIC_VECTOR(31 downto 0);
	signal writereg1: STD_LOGIC_VECTOR(4 downto 0);
	
	signal regwrite2, pcsrc2, jump2, stall2: STD_LOGIC;
	signal instr2, pcaddr2, readdata2, pc2, result2, srca2: STD_LOGIC_VECTOR(31 downto 0);
	signal writereg2: STD_LOGIC_VECTOR(4 downto 0);

	signal regwrite3, pcsrc3, jump3, stall3: STD_LOGIC;
	signal instr3, pcaddr3, readdata3, pc3, result3, srca3: STD_LOGIC_VECTOR(31 downto 0);
	signal writereg3: STD_LOGIC_VECTOR(4 downto 0);
begin
	-- instantiate processor and memories
	mips1: mips port map(clk, reset, pcaddr1, instr1, '0', memwrite1, dataadr1,
		writedata1, readdata1, pc1, regwrite1, writereg1, result1, srca1, jump1, pcsrc1);
	
	mips2: mips port map(clk, reset, pcaddr2, instr2, stall2, memwrite2, dataadr2,
		writedata2, readdata2, pc2, regwrite2, writereg2, result2, srca2, jump2, pcsrc2);
	
	mips3: mips port map(clk, reset, pcaddr3, instr3, stall3, memwrite3, dataadr3,
		writedata3, readdata3, pc3, regwrite3, writereg3, result3, srca3, jump3, pcsrc3);

	
	imem1: imem port map(pc(7 downto 2), instr1, instr2, instr3);

	dmem1: dmem port map(clk, memwrite1, dataadr1, writedata1, readdata1,
							  memwrite2, dataadr2, writedata2, readdata2,
							  memwrite3, dataadr3, writedata3, readdata3);
	

	rf: regfile port map(clk, 
		regwrite1, instr1(25 downto 21), instr1(20 downto 16), writereg1, result1, srca1, writedata1,
		regwrite2, instr2(25 downto 21), instr2(20 downto 16), writereg2, result2, srca2, writedata2,
		regwrite3, instr3(25 downto 21), instr3(20 downto 16), writereg3, result3, srca3, writedata3);

	hazard: hazardunit port map(
		clk, reset, pc, 
		pc1, instr1, jump1, pcsrc1, regwrite1, pcaddr1, 
		pc2, instr2, jump2, pcsrc2, regwrite2, pcaddr2, stall2, 
		pc3, instr3, jump3, pcsrc3, regwrite3, pcaddr3, stall3);

end;