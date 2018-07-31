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