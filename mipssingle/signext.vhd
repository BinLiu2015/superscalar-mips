library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity signext is -- sign extender
  port(
    a: in  STD_LOGIC_VECTOR(15 downto 0);
    y: out STD_LOGIC_VECTOR(31 downto 0)
  );
end;

architecture signext of signext is
begin
  y <= "1111111111111111" & a when a(15) = '1' else "0000000000000000" & a; 
end;