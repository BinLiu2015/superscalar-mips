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