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
		 rd3 	 : out STD_LOGIC_VECTOR(31 downto 0));
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
				if (we1 = '1' and (((a1 /= a2) or (we2 = '0')) and ((a1 /= a3) or (we3 = '0')))) then 
					mem(to_integer(a1(7 downto 2))) := wd1;
				end if;
				if (we2 = '1' and ((a2 /= a3) or (we3 = '0'))) then 
					mem(to_integer(a2(7 downto 2))) := wd2;
				end if;
				if (we3 = '1') then
					mem(to_integer(a3(7 downto 2))) := wd3;
				end if;
			end if;
			rd1 <= mem(to_integer(a1(7 downto 2))); 
			rd2 <= mem(to_integer(a2(7 downto 2))); 
			rd3 <= mem(to_integer(a3(7 downto 2))); 			
			wait on clk, a1, a2, a3;
		end loop;
		
	end process;
end;