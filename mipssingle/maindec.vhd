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