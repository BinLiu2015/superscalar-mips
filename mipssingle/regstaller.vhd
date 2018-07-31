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