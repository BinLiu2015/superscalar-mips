library IEEE; use IEEE.STD_LOGIC_1164.all;

entity controller is -- single cycle control decoder
	port(op, funct : in STD_LOGIC_VECTOR(5 downto 0);
		zero               : in  STD_LOGIC;
		stall			   : in  STD_LOGIC;
		memtoreg, memwrite : out STD_LOGIC;
		pcsrc              : out STD_LOGIC;
		alusrc             : out STD_LOGIC_VECTOR(1 downto 0);
		regdst, regwrite   : out STD_LOGIC;
		jump               : out STD_LOGIC;
		alucontrol         : out STD_LOGIC_VECTOR(2 downto 0));
end;


architecture struct of controller is
	component maindec
		port(op : in STD_LOGIC_VECTOR(5 downto 0);
			memtoreg, memwrite : out STD_LOGIC;
			branch             : out STD_LOGIC;
			alusrc             : out STD_LOGIC_VECTOR(1 downto 0);
			regdst, regwrite   : out STD_LOGIC;
			jump               : out STD_LOGIC;
			aluop              : out STD_LOGIC_VECTOR(1 downto 0);
			branchNotEqual     : out STD_LOGIC);
	end component;
	component aludec
		port(funct : in STD_LOGIC_VECTOR(5 downto 0);
			aluop      : in  STD_LOGIC_VECTOR(1 downto 0);
			alucontrol : out STD_LOGIC_VECTOR(2 downto 0));
	end component;
	signal aluop          : STD_LOGIC_VECTOR(1 downto 0);
	signal branch         : STD_LOGIC;
	signal branchNotEqual : STD_LOGIC;
	signal tmp_memwrite : STD_LOGIC;
	signal tmp_regwrite : STD_LOGIC;
begin
		md : maindec port map(op, memtoreg, tmp_memwrite, branch,
			alusrc, regdst, tmp_regwrite, jump, aluop, branchNotEqual);
		ad : aludec port map(funct, aluop, alucontrol);
	
	memwrite <= tmp_memwrite and not stall;
	regwrite <= tmp_regwrite and not stall;
	pcsrc <= (branch and zero) or (branchNotEqual and not zero);
end;