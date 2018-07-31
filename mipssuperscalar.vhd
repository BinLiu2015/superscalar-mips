-- ==========================================================
-- adder.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity adder is -- adder
  port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
       y:    out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of adder is
begin
  y <= a + b;
end;

-- ==========================================================
-- aludec.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity aludec is -- ALU control decoder
  port(funct : in STD_LOGIC_VECTOR(5 downto 0);
    aluop      : in  STD_LOGIC_VECTOR(1 downto 0);
    alucontrol : out STD_LOGIC_VECTOR(2 downto 0));
end;

architecture behave of aludec is
begin
  process(all) begin
    case aluop is
          when "00"     => alucontrol <= "010"; -- add (for lw/sw/addi)
          when "01"     => alucontrol <= "110"; -- sub (for beq)
          when "11"     => alucontrol <= "001"; -- or (for ori)
          when others   => case funct is -- R-type instructions
            when "100000" => alucontrol <= "010"; -- add 
            when "100010" => alucontrol <= "110"; -- sub
            when "100100" => alucontrol <= "000"; -- and
            when "100101" => alucontrol <= "001"; -- or
            when "101010" => alucontrol <= "111"; -- slt
            when others   => alucontrol   <= "---"; -- ???
        end case;
    end case;
  end process;
end;

-- ==========================================================
-- controller.vhd

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

-- ==========================================================
-- datapath.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all; use IEEE.STD_LOGIC_ARITH.all;
  
entity datapath is -- MIPS datapath
  port(clk, reset : in STD_LOGIC;
    memtoreg, pcsrc   : in     STD_LOGIC;
    alusrc            : in     STD_LOGIC_VECTOR(1 downto 0);
    regdst            : in     STD_LOGIC;
    jump              : in     STD_LOGIC;
    alucontrol        : in     STD_LOGIC_VECTOR(2 downto 0);
    zero              : out    STD_LOGIC;
    pc                : in     STD_LOGIC_VECTOR(31 downto 0);
    instr             : in     STD_LOGIC_VECTOR(31 downto 0);
    aluout            : buffer STD_LOGIC_VECTOR(31 downto 0);
    writedata         : in     STD_LOGIC_VECTOR(31 downto 0);
    readdata          : in     STD_LOGIC_VECTOR(31 downto 0);
    pcnext            : out    STD_LOGIC_VECTOR(31 downto 0);
    writereg          : out    STD_LOGIC_VECTOR(4 downto 0);
    result            : out    STD_LOGIC_VECTOR(31 downto 0);
    srca              : in     STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of datapath is
  component alu
    port(a, b : in STD_LOGIC_VECTOR(31 downto 0);
      alucontrol : in     STD_LOGIC_VECTOR(2 downto 0);
      result     : buffer STD_LOGIC_VECTOR(31 downto 0);
      zero       : out    STD_LOGIC);
  end component;
  -- component regfile
  --   port(clk : in STD_LOGIC;
  --     we3           : in  STD_LOGIC;
  --     ra1, ra2, wa3 : in  STD_LOGIC_VECTOR(4 downto 0);
  --     wd3           : in  STD_LOGIC_VECTOR(31 downto 0);
  --     rd1, rd2      : out STD_LOGIC_VECTOR(31 downto 0));
  -- end component;
  component adder
    port(a, b : in STD_LOGIC_VECTOR(31 downto 0);
      y : out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component sl2
    port(a : in STD_LOGIC_VECTOR(31 downto 0);
      y : out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component signext
    port(a : in STD_LOGIC_VECTOR(15 downto 0);
      y : out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  -- component flopr generic(width :    integer);
  --   port(clk, reset               : in STD_LOGIC;
  --     d : in  STD_LOGIC_VECTOR(width-1 downto 0);
  --     q : out STD_LOGIC_VECTOR(width-1 downto 0));
  -- end component;
  component mux2 generic(width :    integer);
    port(d0, d1                  : in STD_LOGIC_VECTOR(width-1 downto 0);
      s : in  STD_LOGIC;
      y : out STD_LOGIC_VECTOR(width-1 downto 0));
  end component; 
  component mux4 generic (width :    integer);
    port(d0,d1,d2,d3              : in STD_LOGIC_VECTOR(width-1 downto 0);
      s : in  STD_LOGIC_VECTOR(1 downto 0);
      y : out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  -- signal writereg : STD_LOGIC_VECTOR(4 downto 0);
  signal pcjump, 
  pcnextbr, pcplus4, 
  pcbranch                  : STD_LOGIC_VECTOR(31 downto 0);
  signal signimm, signimmsh : STD_LOGIC_VECTOR(31 downto 0);
  signal srcb               : STD_LOGIC_VECTOR(31 downto 0);
begin
  -- next PC logic
  pcjump <= pcplus4(31 downto 28) & instr(25 downto 0) & "00";
    -- pcreg   : flopr generic map(32) port map(clk, reset, pcnext, pc); 
    pcadd1  : adder port map(pc, X"00000004", pcplus4);
    immsh   : sl2 port map(signimm, signimmsh);
    pcadd2  : adder port map(pcplus4, signimmsh, pcbranch);
    pcbrmux : mux2 generic map(32) port map(pcplus4, pcbranch, 
      pcsrc, pcnextbr);
    pcmux : mux2 generic map(32) port map(pcnextbr, pcjump, jump, pcnext);
  
  -- register file logic
    -- rf : regfile port map(clk, regwrite, instr(25 downto 21), 
    --   instr(20 downto 16), writereg, result, srca, 
    --   writedata);
    wrmux : mux2 generic map(5) port map(instr(20 downto 16), 
      instr(15 downto 11), 
      regdst, writereg);
    resmux : mux2 generic map(32) port map(aluout, readdata, 
      memtoreg, result);
    se : signext port map(instr(15 downto 0), signimm);
  
  -- ALU logic
    srcbmux : mux4 generic map(32)
    port map(
      d0 => writedata,
      d1 => "00000000000000000000000000001000",
      d2 => signimm,
      d3 => "0000000000000000" & instr(15 downto 0),
      s  => alusrc,
      y  => srcb
    );
    mainalu : alu port map(srca, srcb, alucontrol, aluout, zero);
end;

-- ==========================================================
-- dmem.vhd

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all; 

entity dmem is -- data memory
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
		 rd3 	 : out STD_LOGIC_VECTOR(31 downto 0);
end;

architecture behave of dmem is

begin
	process is
		type ramtype is array (63 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
		variable mem: ramtype;
	begin
		-- read or write memory
		loop
			if clk'event and clk = '1' then
				if (we1 = '1' and (((a1 /= a2) or (not we2)) and ((a1 /= a3) or (not we3)))) then 
					mem(to_integer(a1(7 downto 2))) := wd1;
				end if;
				if (we2 = '1' and ((a2 /= a3) or (not we3))) then 
					mem(to_integer(a2(7 downto 2))) := wd2;
				end if;
				if (we3 = '1') then
					mem(to_integer(a3(7 downto 2))) := wd3;
				end if;
			end if;
			rd1 <= mem(to_integer(a1(7 downto 2))); 
			rd2 <= mem(to_integer(a2(7 downto 2))); 
			rd3 <= mem(to_integer(a3(7 downto 2))); 
			wait on clk, a;
		end loop;
		
	end process;
end;

-- ==========================================================
-- alu.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity alu is 
  port(a, b : in STD_LOGIC_VECTOR(31 downto 0);
    alucontrol : in     STD_LOGIC_VECTOR(2 downto 0);
    result     : buffer STD_LOGIC_VECTOR(31 downto 0);
    zero       : out    STD_LOGIC);
end;

architecture behave of alu is
  signal condinvb, sum : STD_LOGIC_VECTOR(31 downto 0);
begin
  condinvb <= not b when alucontrol(2) else b;
  sum      <= a + condinvb + alucontrol(2);
  
  process(all) begin
    case alucontrol(1 downto 0) is
      when "00"   => result   <= a and b; 
      when "01"   => result   <= a or b; 
      when "10"   => result   <= sum; 
      when "11"   => result   <= (0 => sum(31), others => '0'); 
      when others => result <= (others => 'X'); 
    end case;
  end process;
  
  zero <= '1' when result = X"00000000" else '0';
end;

-- ==========================================================
-- hazardunit.vhd

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity hazardunit is 
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
end;

architecture test of hazardunit is
	component jumpstaller
		port(jump1, pcsrc1: in STD_LOGIC;
			jump2, pcsrc2: in STD_LOGIC;
			jump3, pcsrc3: in STD_LOGIC;
			stall2, stall3: out STD_LOGIC);
	end component;
	component memorystaller
        port(op1    : in  STD_LOGIC_VECTOR(5 downto 0);
            basereg1  : in  STD_LOGIC_VECTOR(4 downto 0);
            offset1   : in  STD_LOGIC_VECTOR(15 downto 0);
            op2       : in  STD_LOGIC_VECTOR(5 downto 0);
            basereg2  : in  STD_LOGIC_VECTOR(4 downto 0);
            offset2   : in  STD_LOGIC_VECTOR(15 downto 0);
            op3       : in  STD_LOGIC_VECTOR(5 downto 0);
            basereg3  : in  STD_LOGIC_VECTOR(4 downto 0);
            offset3   : in  STD_LOGIC_VECTOR(15 downto 0);
            stall2, stall3: out STD_LOGIC);      
  	end component;
	component regstaller
        port(writereg1    : in  STD_LOGIC;
            instr1        : in  STD_LOGIC;
            writereg2     : in  STD_LOGIC;
            instr2        : in  STD_LOGIC;
            writereg3     : in  STD_LOGIC;
            instr3        : in  STD_LOGIC;
            stall2, stall3: out STD_LOGIC);      
  	end component;
    component flopr generic(width :    integer);
		port(clk, reset               : in STD_LOGIC;
		d : in  STD_LOGIC_VECTOR(width-1 downto 0);
		q : out STD_LOGIC_VECTOR(width-1 downto 0));
  	end component;

	signal pc, pcnext, instr,
	readdata, result, srca: STD_LOGIC_VECTOR(31 downto 0);

	signal writereg: STD_LOGIC_VECTOR(4 downto 0);

	signal jump1: STD_LOGIC;
	signal pcsrc1: STD_LOGIC;
	signal regwrite: STD_LOGIC;
	signal jump_stall2, mem_stall2, reg_stall2: STD_LOGIC;
	signal jump_stall3, mem_stall3, reg_stall3: STD_LOGIC;
begin
    stall2 <= jump_stall2 or mem_stall2 or reg_stall2;
    stall3 <= jump_stall3 or mem_stall3 or reg_stall3 or stall2;

    pcnext <= pc1 when stall2 and stall3 else
              pc2 when stall3 else
              pc3;

    pcaddr1 <= pc;
    pcaddr2 <= pc + "100";
    pcaddr3 <= pc + "1000";


    js: jumpstaller port map(
        jump1 => jump1,
        pcsrc1 => pcsrc1,
        jump2 => jump2,
        pcsrc2 => pcsrc2,
        jump3 => jump3,
        pcsrc3 => pcsrc3,
        stall2 => jump_stall2,
        stall3 => jump_stall3
        );

    rs: regstaller port map(
        writereg1 => writereg1,
        instr1 => instr1,
        writereg2 => writereg2,
        instr2 => instr2,
        writereg3 => writereg3,
        instr3 => instr3,
        stall2 => reg_stall2,
        stall3 => reg_stall3
        );
    
    ms: memorystaller port map(
        op1 => instr1(31 downto 26),
        basereg1 => instr1(25 downto 21),
        offset1 => instr1(15 downto 0),
        op2 => instr2(31 downto 26),
        basereg2 => instr2(25 downto 21),
        offset2 => instr2(15 downto 0),
        op3 => instr3(31 downto 26),
        basereg3 => instr3(25 downto 21),
        offset3 => instr3(15 downto 0),
        stall2 => mem_stall2,
        stall3 => mem_stall3
    );

    pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc); 
end;

-- ==========================================================
-- imem.vhd

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all; 

entity imem is -- instruction memory
	port(a: in STD_LOGIC_VECTOR(5 downto 0);
		rd1: out STD_LOGIC_VECTOR(31 downto 0);
		rd2: out STD_LOGIC_VECTOR(31 downto 0);
		rd3: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of imem is
begin
	process is
		file mem_file             : TEXT;
		variable L                : line;
		variable ch               : character;
		variable i, index, result : integer;
		type ramtype is array (63 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
		variable mem: ramtype;
	begin
		-- initialize memory from file
		for i in 0 to 63 loop -- set all contents low
			mem(i) := (others => '0');
		end loop;
		index := 0;
		FILE_OPEN(mem_file, "C:\Users\agodinho\Documents\Arquitetura2\memfile2.dat", READ_MODE);
		while not endfile(mem_file) loop
		readline(mem_file, L);
		result := 0;
		for i in 1 to 8 loop
			read(L, ch);
			if '0' <= ch and ch <= '9' then 
				result := character'pos(ch) - character'pos('0');
			elsif 'a' <= ch and ch <= 'f' then
				result := character'pos(ch) - character'pos('a')+10;
			else report "Format error on line " & integer'image(index)
				severity error;
			end if;
			mem(index)(35-i*4 downto 32-i*4) := to_std_logic_vector(result,4);
		end loop;
		index := index + 1;
	end loop;
	
	-- read memory
	loop
		rd1 <= mem(to_integer(a));
		rd2 <= mem(to_integer(a) + 4);
		rd3 <= mem(to_integer(a) + 8);
		wait on a;
	end loop;
end process;
end;

-- ==========================================================
-- jumpstaller.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity jumpstaller is
  port(jump1, pcsrc1: in STD_LOGIC;
       jump2, pcsrc2: in STD_LOGIC;
       jump3, pcsrc3: in STD_LOGIC;
       stall2, stall3: out STD_LOGIC);
end;

architecture behave of jumpstaller is
begin
  stall2 <= jump1 or pcsrc1; 
  stall3 <= jump2 or pcsrc2 or jump1 or pcsrc1; 
end;

-- ==========================================================
-- flopr.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;  use IEEE.STD_LOGIC_ARITH.all;

entity flopr is -- flip-flop with synchronous reset
  generic(width: integer);
  port(clk, reset: in  STD_LOGIC;
       d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
       q:          out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture asynchronous of flopr is
begin
  process(clk, reset) begin
    if reset then  q <= (others => '0');
    elsif rising_edge(clk) then
      q <= d;
    end if;
  end process;
end;

-- ==========================================================
-- maindec.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity maindec is -- main control decoder
	port(op : in STD_LOGIC_VECTOR(5 downto 0);
		memtoreg, memwrite : out STD_LOGIC;
		branch             : out STD_LOGIC; 
		alusrc             : out STD_LOGIC_VECTOR(1 downto 0);
		regdst, regwrite   : out STD_LOGIC;
		jump               : out STD_LOGIC;
		aluop              : out STD_LOGIC_VECTOR(1 downto 0);
		branchNotEqual     : out STD_LOGIC);
end;

architecture behave of maindec is
	signal controls : STD_LOGIC_VECTOR(10 downto 0);
begin
	process(all) begin
		case op is
			 when "000000" => controls <= "11000000100"; -- RTYPE
			 when "100011" => controls <= "10100010000"; -- LW
			 when "101011" => controls <= "00100100000"; -- SW
			 when "000100" => controls <= "00001000010"; -- BEQ
			 when "000101" => controls <= "00001000011"; -- BNE
			 when "001000" => controls <= "10100000000"; -- ADDI
			 when "000010" => controls <= "00000001000"; -- J
			 when "001101" => controls <= "10110000110"; -- ORI
			 when others   => controls   <= "-----------"; -- illegal op
		end case;
	end process;
	
	(regwrite, regdst, alusrc, branch, memwrite,
		memtoreg, jump, aluop(1 downto 0), branchNotEqual) <= controls;
end;

-- ==========================================================
-- mips.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mips is -- single cycle MIPS processor
	port(clk, reset : in STD_LOGIC;
		pc                : in STD_LOGIC_VECTOR(31 downto 0);
		instr             : in  STD_LOGIC_VECTOR(31 downto 0);
		stall			  : in STD_LOGIC;
		memwrite          : out STD_LOGIC;
		aluout			  : out STD_LOGIC_VECTOR(31 downto 0);
		writedata		  : in  STD_LOGIC_VECTOR(31 downto 0);
		readdata          : in  STD_LOGIC_VECTOR(31 downto 0);
		pcnext            : out STD_LOGIC_VECTOR(31 downto 0);
		regwrite          : out STD_LOGIC;
		writereg          : out STD_LOGIC_VECTOR(4 downto 0);
		result	          : out STD_LOGIC_VECTOR(31 downto 0);
		srca              : in  STD_LOGIC_VECTOR(31 downto 0);
		jump              : buffer  STD_LOGIC;
		pcsrc             : buffer  STD_LOGIC);
end;

architecture struct of mips is
	component controller
		port(op, funct : in STD_LOGIC_VECTOR(5 downto 0);
			zero               : in  STD_LOGIC;
			stall			   : in  STD_LOGIC;
			memtoreg, memwrite : out STD_LOGIC;
			pcsrc              : out STD_LOGIC;
			alusrc             : out STD_LOGIC_VECTOR(1 downto 0);
			regdst, regwrite   : out STD_LOGIC;
			jump               : out STD_LOGIC;
			alucontrol         : out STD_LOGIC_VECTOR(2 downto 0));
	end component;
	component datapath
		port(clk, reset : in STD_LOGIC;
			memtoreg, pcsrc   : in     STD_LOGIC;
			alusrc            : in     STD_LOGIC_VECTOR(1 downto 0);
			regdst            : in     STD_LOGIC;
			jump 			  : in     STD_LOGIC;
			alucontrol        : in     STD_LOGIC_VECTOR(2 downto 0);
			zero              : out    STD_LOGIC;
			pc                : in     STD_LOGIC_VECTOR(31 downto 0);
			instr             : in     STD_LOGIC_VECTOR(31 downto 0);
			aluout			  : buffer STD_LOGIC_VECTOR(31 downto 0);
			writedata		  : in     STD_LOGIC_VECTOR(31 downto 0);
			readdata          : in     STD_LOGIC_VECTOR(31 downto 0);
			pcnext            : out    STD_LOGIC_VECTOR(31 downto 0);
			writereg          : out    STD_LOGIC_VECTOR(4 downto 0);
			result	          : out    STD_LOGIC_VECTOR(31 downto 0);
			srca              : in     STD_LOGIC_VECTOR(31 downto 0));
	end component;
	
	signal alusrc                                  : STD_LOGIC_VECTOR(1 downto 0);
	signal memtoreg, regdst 				   : STD_LOGIC;
	signal zero                                    : STD_LOGIC;
	signal alucontrol                              : STD_LOGIC_VECTOR(2 downto 0);
begin
		cont : controller port map(instr(31 downto 26), instr(5 downto 0),
			zero, stall, memtoreg, memwrite, pcsrc, alusrc,
			regdst, regwrite, jump, alucontrol);
		dp : datapath port map(clk, reset, memtoreg, pcsrc, alusrc, regdst,
			jump, alucontrol, zero, pc, instr,
			aluout, writedata, readdata, pcnext, writereg, result, srca);
end;

-- ==========================================================
-- mux2.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mux2 is -- two-input multiplexer
  generic(width: integer);
  port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
       s:      in  STD_LOGIC;
       y:      out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux2 is
begin
  y <= d1 when s else d0;
end;

-- ==========================================================
-- regfile.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity regfile is -- three-port register file
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
end;

architecture behave of regfile is
  type ramtype is array (31 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
  signal mem: ramtype;
begin
  -- three-ported register file
  -- read two ports combinationally
  -- write third port on rising edge of clock
  -- register 0 hardwired to 0
  -- note: for pipelined processor, write third port
  -- on falling edge of clk
  process(clk) begin
    if rising_edge(clk) then
       if clk'event and clk = '1' then
				if (writeEn1 = '1' and (((wa1 /= wa2) or (not writeEn2)) and ((wa1 /= wa3) or (not writeEn3)))) then 
					mem(to_integer(wa1)) := writedata1;
				end if;
				if (writeEn2 = '1' and ((wa2 /= wa3) or (not writeEn3))) then 
					mem(to_integer(wa2)) := writedata2;
				end if;
				if (writeEn3 = '1') then
					mem(to_integer(wa3)) := writedata3;
				end if;
			end if;
			rd1 <= mem(to_integer(a1)); 
			rd2 <= mem(to_integer(a2)); 
			rd3 <= mem(to_integer(a3)); 
    end if;
  end process;
  process(all) begin
    if (to_integer(ra1A) = 0) then read1A <= X"00000000"; -- register 0 holds 0
    else read1A <= mem(to_integer(ra1A));
    end if;
    if (to_integer(ra1B) = 0) then read1B <= X"00000000"; 
    else read1B <= mem(to_integer(ra1B));
    end if;

    if (to_integer(ra2A) = 0) then read2A <= X"00000000"; -- register 0 holds 0
    else read2A <= mem(to_integer(ra2A));
    end if;
    if (to_integer(ra2B) = 0) then read2B <= X"00000000"; 
    else read2B <= mem(to_integer(ra2B));
    end if;

    if (to_integer(ra3A) = 0) then read3A <= X"00000000"; -- register 0 holds 0
    else read3A <= mem(to_integer(ra3A));
    end if;
    if (to_integer(ra3B) = 0) then read3B <= X"00000000"; 
    else read3B <= mem(to_integer(ra3B));
    end if;
    
  end process;
end;

-- ==========================================================
-- memorystaller.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity memorystaller is
 port(op1       : in  STD_LOGIC_VECTOR(5 downto 0);
      basereg1  : in  STD_LOGIC_VECTOR(4 downto 0);
      offset1   : in  STD_LOGIC_VECTOR(15 downto 0);
      op2       : in  STD_LOGIC_VECTOR(5 downto 0);
      basereg2  : in  STD_LOGIC_VECTOR(4 downto 0);
      offset2   : in  STD_LOGIC_VECTOR(15 downto 0);
      op3       : in  STD_LOGIC_VECTOR(5 downto 0);
      basereg3  : in  STD_LOGIC_VECTOR(4 downto 0);
      offset3   : in  STD_LOGIC_VECTOR(15 downto 0);
      stall2, stall3: out STD_LOGIC);
end;

architecture behave of memorystaller is
begin
  stall2 <= '1' when (op1 = "101011" and op2 = "100011" and basereg1 = basereg2 and offset1 = offset2) else '0';
  stall3 <= '1' when (op2 = "101011" and op3 = "100011" and basereg2 = basereg3 and offset2 = offset3) else '0';
end; 

-- ==========================================================
-- mux4.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;
    
entity mux4 is -- four-input multiplexer
    generic(width    :    integer);
    port(d0,d1,d2,d3 : in STD_LOGIC_VECTOR(width-1 downto 0);
        s : in  STD_LOGIC_VECTOR(1 downto 0);
        y : out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux4 is
begin
    y <= d0 when s="00" else
        d1     when s="01" else
        d2     when s="10" else
        d3;
end;

-- ==========================================================
-- testbench.vhd

-- mips.vhd
-- From Section 7.6 of Digital Design & Computer Architecture
-- Updated to VHDL 2008 26 July 2011 David_Harris@hmc.edu

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity testbench is
end;

architecture test of testbench is
  component top
    port(clk, reset:           in  STD_LOGIC;
         writedata, dataadr:   out STD_LOGIC_VECTOR(31 downto 0);
         memwrite:             out STD_LOGIC);
  end component;
  signal writedata1, dataadr1:    STD_LOGIC_VECTOR(31 downto 0);
  signal writedata2, dataadr2:    STD_LOGIC_VECTOR(31 downto 0);
  signal writedata3, dataadr3:    STD_LOGIC_VECTOR(31 downto 0);
  signal clk, reset,  memwrite1, memwrite2, memwrite3: STD_LOGIC;
begin

  -- instantiate device to be tested
  dut: top port map(clk, reset, 
                    writedata1, dataadr1,
                    writedata2, dataadr2,
                    writedata3, dataadr3,
                    memwrite1,
                    memwrite2,
                    memwrite3);

  -- Generate clock with 10 ns period
  process begin
    clk <= '1';
    wait for 5 ns; 
    clk <= '0';
    wait for 5 ns;
  end process;

  -- Generate reset for first two clock cycles
  process begin
    reset <= '1';
    wait for 22 ns;
    reset <= '0';
    wait;
  end process;

	-- check that -33022 gets written to address 84 at end of program
  process (clk) begin
    if (clk'event and clk = '0' and memwrite1 = '1') then
			if (to_integer(dataadr1) = 84 and to_integer(writedata1) = -33022) then 
				report "NO ERRORS : Simulation succeeded1" severity failure;
      elsif (dataadr1 /= 80) then 
        report "Simulation failed" severity failure;
      end if;
    end if;

    if (clk'event and clk = '0' and memwrite2 = '1') then
			if (to_integer(dataadr2) = 84 and to_integer(writedata2) = -33022) then 
				report "NO ERRORS : Simulation succeeded2" severity failure;
      elsif (dataadr2 /= 80) then 
        report "Simulation failed" severity failure;
      end if;
    end if;

    if (clk'event and clk = '0' and memwrite3 = '1') then
			if (to_integer(dataadr3) = 84 and to_integer(writedata3) = -33022) then 
				report "NO ERRORS : Simulation succeeded3" severity failure;
      elsif (dataadr3 /= 80) then 
        report "Simulation failed" severity failure;
      end if;
    end if;
  end process;
end;

-- ==========================================================
-- top.vhd

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
	port(clk, reset: in STD_LOGIC;
		writedata1, dataadr1: buffer STD_LOGIC_VECTOR(31 downto 0);
		writedata2, dataadr2: buffer STD_LOGIC_VECTOR(31 downto 0);
		writedata3, dataadr3: buffer STD_LOGIC_VECTOR(31 downto 0);
		memwrite1          : buffer STD_LOGIC);
		memwrite2          : buffer STD_LOGIC);
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
			 rd3 	 : out STD_LOGIC_VECTOR(31 downto 0);
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
	signal pc, pcnext, instr1, instr2, instr3,
	readdata, result, srca: STD_LOGIC_VECTOR(31 downto 0);

	signal writereg: STD_LOGIC_VECTOR(4 downto 0);

	signal jump1: STD_LOGIC;
	signal pcsrc1: STD_LOGIC;
	signal regwrite: STD_LOGIC;

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
		pc1, instr1, jump1, pcsrc1, writereg1, pcaddr1, 
		pc2, instr2, jump2, pcsrc2, writereg2, pcaddr2, stall2, 
		pc3, instr3, jump3, pcsrc3, writereg3, pcaddr3, stall3)

end;

-- ==========================================================
-- signext.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity signext is -- sign extender
  port(a: in  STD_LOGIC_VECTOR(15 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of signext is
begin
  y <= X"ffff" & a when a(15) else X"0000" & a; 
end;

-- ==========================================================
-- regstaller.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity regstaller is
  port(writereg1     : in  STD_LOGIC;
       instr1        : in  STD_LOGIC_VECTOR(31 downto 0);
       writereg2     : in  STD_LOGIC;
       instr2        : in  STD_LOGIC_VECTOR(31 downto 0);
       writereg3     : in  STD_LOGIC;
       instr3        : in  STD_LOGIC_VECTOR(31 downto 0);
       stall2, stall3: out STD_LOGIC); 
end;

architecture behave of regstaller is
    signal dest1: STD_LOGIC_VECTOR(4 downto 0);
    signal srcA2: STD_LOGIC_VECTOR(4 downto 0);
    signal srcB2: STD_LOGIC_VECTOR(4 downto 0);
    signal dest2: STD_LOGIC_VECTOR(4 downto 0);
    signal srcA3: STD_LOGIC_VECTOR(4 downto 0);
    signal srcB3: STD_LOGIC_VECTOR(4 downto 0);
begin

    dest1 <= instr1(15 downto 11) when instr1(31 downto 26) = "000000" else instr1(20 downto 16);
    srcA2 <= instr2(26 downto 21);
    srcB2 <= instr2(20 downto 16) when instr2(31 downto 26) = "000000" else instr2(26 downto 21);

    dest2 <= instr2(15 downto 11) when instr2(31 downto 26) = "000000" else instr2(20 downto 16);
    srcA3 <= instr3(26 downto 21);
    srcB3 <= instr3(20 downto 16) when instr3(31 downto 26) = "000000" else instr3(26 downto 21);

    stall2 <= '1' when (writereg1 and (dest1 = srcA2 or dest1 = srcB2)) else '0';
    stall3 <= '1' when (writereg2 and (dest2 = srcA3 or dest2 = srcB3)) else '0';
end;

-- ==========================================================
-- sl2.vhd

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity sl2 is -- shift left by 2
  port(a: in  STD_LOGIC_VECTOR(31 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of sl2 is
begin
  y <= a(29 downto 0) & "00";
end;

