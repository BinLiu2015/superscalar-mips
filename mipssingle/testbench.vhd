-- mips.vhd
-- From Section 7.6 of Digital Design & Computer Architecture
-- Updated to VHDL 2008 26 July 2011 David_Harris@hmc.edu

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity testbench is
end;

architecture test of testbench is
  component top
    port(clk, reset : in STD_LOGIC; 
      writedata1, dataadr1 : buffer STD_LOGIC_VECTOR(31 downto 0);
      writedata2, dataadr2 : buffer STD_LOGIC_VECTOR(31 downto 0);
      writedata3, dataadr3 : buffer STD_LOGIC_VECTOR(31 downto 0);
      memwrite1            : buffer STD_LOGIC;
      memwrite2            : buffer STD_LOGIC;
      memwrite3            : buffer STD_LOGIC);
  end component;
  signal writedata1, dataadr1                        : STD_LOGIC_VECTOR(31 downto 0);
  signal writedata2, dataadr2                        : STD_LOGIC_VECTOR(31 downto 0);
  signal writedata3, dataadr3                        : STD_LOGIC_VECTOR(31 downto 0);
  signal clk, reset, memwrite1, memwrite2, memwrite3 : STD_LOGIC;
begin
  
  -- instantiate device to be tested
    dut : top port map(clk, reset, 
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