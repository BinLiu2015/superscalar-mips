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
				if (writeEn1 = '1' and (((wa1 /= wa2) or (writeEn2 = '0')) and ((wa1 /= wa3) or (writeEn3 = '0')))) then 
					mem(to_integer(wa1)) <= writedata1;
				end if;
				if (writeEn2 = '1' and ((wa2 /= wa3) or (writeEn3 = '0'))) then 
					mem(to_integer(wa2)) <= writedata2;
				end if;
				if (writeEn3 = '1') then
					mem(to_integer(wa3)) <= writedata3;
				end if;
			end if;
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