-- Projeto pipeline_mips da disciplina PCS 3612 – Organização e Arquitetura de Computadores I
-- Profa. Dra. Cíntia Borges Margi
-- Alunos do grupo:
-- Vinicius Oliveira Santos				- NUSP: 10336636
-- Beogival Wagner Lucas Santos Junior	- NUSP: 08992836

-- mips.vhd
-- From Section 7.6 of Digital Design & Computer Architecture
-- Updated to VHDL 2008 26 July 2011 David_Harris@hmc.edu	  

library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity testbench is
end;

architecture test of testbench is
	component top
		port(clk, reset:          in  STD_LOGIC;
			writedata, dataadr:   out STD_LOGIC_VECTOR(31 downto 0);
			memwrite:             out STD_LOGIC);
	end component;
	signal writedata, dataadr:    STD_LOGIC_VECTOR(31 downto 0);
	signal clk, reset,  memwrite: STD_LOGIC;
begin
	
	-- instantiate device to be tested
	dut: top port map(clk, reset, writedata, dataadr, memwrite);
	
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
	
	-- check that 7 gets written to address 84 at end of program
	process (clk) begin
		if (clk'event and clk = '0' and memwrite = '1') then
			if (to_integer(dataadr) = 84 and to_integer(writedata) = 7) then
				report "NO ERRORS: Simulation succeeded" severity failure;
			elsif (dataadr /= 80) then
				report "Simulation failed" severity failure;
			end if;
		end if;
	end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
	port(clk, reset:           in     STD_LOGIC;
		writedata, dataadr:   buffer STD_LOGIC_VECTOR(31 downto 0);
		memwrite:             buffer STD_LOGIC);
end;

architecture test of top is
	component mips
		port(clk, reset:        in  STD_LOGIC;
			pc:                out STD_LOGIC_VECTOR(31 downto 0);
			instr:             in  STD_LOGIC_VECTOR(31 downto 0);
			memwrite:          out STD_LOGIC;
			aluout, writedata: out STD_LOGIC_VECTOR(31 downto 0);
			readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component imem
		port(a:  in  STD_LOGIC_VECTOR(5 downto 0);
			rd: out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component dmem
		port(clk, we:  in STD_LOGIC;
			a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
			rd:       out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	signal pc, instr,
	readdata: STD_LOGIC_VECTOR(31 downto 0);
begin
	-- instantiate processor and memories
	mips1: mips port map(clk, reset, pc, instr, memwrite, dataadr,
		writedata, readdata);
	imem1: imem port map(pc(7 downto 2), instr);
	dmem1: dmem port map(clk, memwrite, dataadr, writedata, readdata);
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity dmem is -- data memory
	port(clk, we:  in STD_LOGIC;
		a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
		rd:       out STD_LOGIC_VECTOR(31 downto 0));
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
				if (we = '1') then mem(to_integer(a(7 downto 2))) := wd;
				end if;
			end if;
			rd <= mem(to_integer(a(7 downto 2)));
			wait on clk, a;
		end loop;
		
	end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity imem is -- instruction memory
	port(a:  in  STD_LOGIC_VECTOR(5 downto 0);
		rd: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of imem is
begin
	process is
		file mem_file: TEXT;
		variable L: line;
		variable ch: character;
		variable i, index, result: integer;
		type ramtype is array (63 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
		variable mem: ramtype;
	begin
		-- initialize memory from file
		for i in 0 to 63 loop -- set all contents low
			mem(i) := (others => '0');
		end loop;
		index := 0;
		FILE_OPEN(mem_file, "memfile.dat", READ_MODE);
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
				mem(index)(35-i*4 downto 32-i*4) :=to_std_logic_vector(result,4);
			end loop;
			index := index + 1;
		end loop;
		
		-- read memory
		loop
			rd <= mem(to_integer(a));
			wait on a;
		end loop;
	end process;
end;

library IEEE; use IEEE.std_logic_1164.all;

entity registrador_n is
	generic (
		constant N: integer := 8 );
	port (clock, clear, enable: in std_logic;
		D: in std_logic_vector(N-1 downto 0);
		Q: out std_logic_vector (N-1 downto 0) );
end registrador_n;

architecture registrador_n of registrador_n is
	signal IQ: std_logic_vector(N-1 downto 0);
begin
	
	process(clock, clear, enable, IQ)
	begin
		if (clear = '1') then IQ <= (others => '0');
		elsif (clock'event and clock='1') then
			if (enable='1') then IQ <= D; end if;
		end if;
		Q <= IQ;
	end process;
	
end registrador_n;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mips is -- single cycle MIPS processor
	port(clk, reset:        in  STD_LOGIC;
		pc:                out STD_LOGIC_VECTOR(31 downto 0);
		instr:             in  STD_LOGIC_VECTOR(31 downto 0);
		memwrite:          out STD_LOGIC;
		aluout, writedata: out STD_LOGIC_VECTOR(31 downto 0);
		readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of mips is
	component controller
		port(op, funct:           in  STD_LOGIC_VECTOR(5 downto 0);
			clk:                 in  STD_LOGIC;
			equalD:              in  STD_LOGIC;
			memtoRegE, memtoRegM:out STD_LOGIC;
			regwriteE, regwriteM:out STD_LOGIC;
			branchD:             out STD_LOGIC;
			branch_notequalD:    out STD_LOGIC;
			memtoreg, memwrite:  out STD_LOGIC;
			pcsrc, alusrc:       out STD_LOGIC;
			regdst, regwrite:    out STD_LOGIC;
			jump:                out STD_LOGIC;
			alucontrol:          out STD_LOGIC_VECTOR(2 downto 0));
	end component;
	component datapath
		port(clk, reset:          in  STD_LOGIC;
			memtoreg, pcsrc:     in  STD_LOGIC;
			alusrc, regdst:      in  STD_LOGIC;
			regwrite, jump:      in  STD_LOGIC;
			memtoRegE, memtoRegM:in  STD_LOGIC;
			regwriteE, regwriteM:in  STD_LOGIC;
			branchD:             in STD_LOGIC;
			branch_notequalD:    in STD_LOGIC;
			equalD,stallD:       out STD_LOGIC;
			alucontrol:          in  STD_LOGIC_VECTOR(2 downto 0);
			pc:                  buffer STD_LOGIC_VECTOR(31 downto 0);
			instr:               in STD_LOGIC_VECTOR(31 downto 0);
			aluout, writedata:   buffer STD_LOGIC_VECTOR(31 downto 0);
			readdata:            in  STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component registrador_n
		generic (
			constant N: integer := 16 );
		port (
			clock, clear, enable: in std_logic;
			D: in std_logic_vector(N-1 downto 0);
			Q: out std_logic_vector (N-1 downto 0) );
	end component;
	signal memtoreg, alusrc, regdst, regwrite, jump, pcsrc: STD_LOGIC;
	signal alucontrol:                                      STD_LOGIC_VECTOR(2 downto 0);
	signal instrD:                                          STD_LOGIC_VECTOR(31 downto 0);
	signal memtoRegE, memtoRegM:                            STD_LOGIC;
	signal regwriteE, regwriteM:                            STD_LOGIC;
	signal regwriteW, branchD:                              STD_LOGIC;
	signal branch_notequalD:                                STD_LOGIC;
	signal equalD,stallD:                                   STD_LOGIC;
	
begin
	cont: controller port map(instrD(31 downto 26), instrD(5 downto 0),
		clk, equalD, memtoRegE, memtoRegM, regwriteE, regwriteM,
		branchD, branch_notequalD, memtoreg, memwrite, pcsrc, alusrc,
		regdst, regwrite, jump, alucontrol);
	dp: datapath port map(clk, reset, memtoreg, pcsrc, alusrc, regdst,
		regwrite, jump, memtoRegE, memtoRegM, regwriteE, regwriteM, branchD,
		branch_notequalD, equalD, stallD, alucontrol, pc, instrD,
		aluout, writedata, readdata);
	regD: registrador_n generic map (N => 32) port map(clk, pcsrc, not(stallD), instr, instrD);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity controller is -- single cycle control decoder
	port(op, funct:           in  STD_LOGIC_VECTOR(5 downto 0);
		clk:                 in  STD_LOGIC;
		equalD:              in  STD_LOGIC;
		memtoRegE, memtoRegM:out STD_LOGIC;
		regwriteE, regwriteM:out STD_LOGIC;
		branchD:             out STD_LOGIC;
		branch_notequalD:    out STD_LOGIC;
		memtoreg, memwrite:  out STD_LOGIC;
		pcsrc, alusrc:       out STD_LOGIC;
		regdst, regwrite:    out STD_LOGIC;
		jump:                out STD_LOGIC;
		alucontrol:          out STD_LOGIC_VECTOR(2 downto 0));
end;


architecture struct of controller is
	component maindec
		port(op:                 				  in  STD_LOGIC_VECTOR(5 downto 0);
			memtoreg, memwrite: 				  out STD_LOGIC;
			branch, alusrc, branch_notequal:     out STD_LOGIC;
			regdst, regwrite:   				  out STD_LOGIC;
			jump:               				  out STD_LOGIC;
			aluop:              				  out STD_LOGIC_VECTOR(1 downto 0));
	end component;
	component aludec
		port(funct:      in  STD_LOGIC_VECTOR(5 downto 0);
			aluop:      in  STD_LOGIC_VECTOR(1 downto 0);
			alucontrol: out STD_LOGIC_VECTOR(2 downto 0));
	end component;
	component registrador_n
		generic (
			constant N: integer := 16 );
		port (
			clock, clear, enable: in std_logic;
			D: in std_logic_vector(N-1 downto 0);
			Q: out std_logic_vector (N-1 downto 0) );
	end component;
	signal aluop                                                                                   : STD_LOGIC_VECTOR(1 downto 0);
	signal RegWriteD, MemtoRegD, MemWriteD, s_BranchD, ALUSrcD, RegDstD, BranchNotEqualD		   : STD_LOGIC;
	signal ALUControlD																			   : STD_LOGIC_VECTOR(2 downto 0);
	signal s_regE                                                                                  : STD_LOGIC_VECTOR(9 downto 0);
	signal s_regM                                                                                  : STD_LOGIC_VECTOR(4 downto 0);
	signal s_regW                                                                                  : STD_LOGIC_VECTOR(1 downto 0);
	signal branch                                                                                  : STD_LOGIC;
	signal branch_notequal                                                                         : STD_LOGIC;
	
begin
	md: maindec port map(op, MemtoRegD, MemWriteD, s_BranchD,
		ALUSrcD, BranchNotEqualD, RegDstD, RegWriteD, jump, aluop);
	ad: aludec port map(funct, aluop, ALUControlD);
	
	regE: registrador_n generic map (N => 10) port map (clk, '0', '1', RegWriteD & MemtoRegD & MemWriteD & s_BranchD & ALUControlD & ALUSrcD & RegDstD & BranchNotEqualD, s_regE);
	
	regM: registrador_n generic map (N => 5) port map (clk, '0', '1', s_regE(3 downto 0) & s_regE(9), s_regM);
	
	regW: registrador_n generic map (N => 2) port map (clk, '0', '1', s_regM(1 downto 0), s_regW);
	
	pcsrc <= (branch and equalD) or (branch_notequal and not(equalD));
	
	branchD <= s_BranchD;
	branch_notequalD <= BranchNotEqualD;
	
	
	-- Verificar se os vetores estão sendo alocados na ordem correta
	-- Depois do primeiro registrador
	alucontrol       <= s_regE(6 downto 4); 
	alusrc           <= s_regE(7);
	regdst           <= s_regE(8);
	memtoRegE        <= s_regE(1);
	regwriteE        <= s_regE(0);
	
	-- Depois do segundo registrador
	memwrite         <= s_regM(2);
	branch           <= s_regM(3);
	branch_notequal  <= s_regM(4);
	memtoRegM        <= s_regM(1);
	regwriteM        <= s_regM(0);
	
	-- Depois do terceiro registrador
	regwrite         <= s_regW(0);
	memtoreg         <= s_regW(1);
	
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity maindec is -- main control decoder
	port(op:                 					in  STD_LOGIC_VECTOR(5 downto 0);
		memtoreg, memwrite: 					out STD_LOGIC;
		branch, alusrc, branch_notequal:        out STD_LOGIC;
		regdst, regwrite:   					out STD_LOGIC;
		jump:               					out STD_LOGIC;
		aluop:              					out STD_LOGIC_VECTOR(1 downto 0));
end;

architecture behave of maindec is
	signal controls: STD_LOGIC_VECTOR(9 downto 0);
begin
	process(all) begin
		case op is
			when "000000" => controls <= "1100000010"; -- RTYPE
			when "100011" => controls <= "1010001000"; -- LW
			when "101011" => controls <= "0010010000"; -- SW
			when "000100" => controls <= "0000100001"; -- BEQ
			when "000101" => controls <= "0001000001"; -- NOT EQUAL
			when "001000" => controls <= "1010000000"; -- ADDI
			when "001101" => controls <= "1010000011"; -- ORI OR - I
			when "000010" => controls <= "0000000100"; -- J
			when others   => controls <= "----------"; -- illegal op
		end case;
	end process;
	
	-- (regwrite, regdst, alusrc, branch, memwrite,
	--  memtoreg, jump, aluop(1 downto 0)) <= controls;
	(regwrite, regdst, alusrc, branch_notequal, branch, memwrite,
	memtoreg, jump) <= controls(9 downto 2);
	aluop <= controls(1 downto 0);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity aludec is -- ALU control decoder
	port(funct:      in  STD_LOGIC_VECTOR(5 downto 0);
		aluop:      in  STD_LOGIC_VECTOR(1 downto 0);
		alucontrol: out STD_LOGIC_VECTOR(2 downto 0));
end;

architecture behave of aludec is
begin
	process(all) begin
		case aluop is
			when "00" => alucontrol <= "010"; -- add (for lw/sw/addi)
			when "01" => alucontrol <= "110"; -- sub (for beq)
			when "11" => alucontrol <= "001"; -- or
			when others => case funct is      -- R-type instructions
					when "100000" => alucontrol <= "010"; -- add
					when "100010" => alucontrol <= "110"; -- sub
					when "100100" => alucontrol <= "000"; -- and
					when "100101" => alucontrol <= "001"; -- or
					when "101010" => alucontrol <= "111"; -- slt
					when others   => alucontrol <= "---"; -- ???
			end case;
		end case;
	end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; use IEEE.STD_LOGIC_ARITH.all;

entity datapath is  -- MIPS datapath
	port(clk, reset:          in  STD_LOGIC;
		memtoreg, pcsrc:     in  STD_LOGIC;
		alusrc, regdst:      in  STD_LOGIC;
		regwrite, jump:      in  STD_LOGIC;
		memtoRegE, memtoRegM:in  STD_LOGIC;
		regwriteE, regwriteM:in  STD_LOGIC;
		branchD:             in STD_LOGIC;
		branch_notequalD:    in STD_LOGIC;
		equalD, stallD:      out STD_LOGIC;
		alucontrol:          in  STD_LOGIC_VECTOR(2 downto 0);
		pc:                  buffer STD_LOGIC_VECTOR(31 downto 0);
		instr:               in  STD_LOGIC_VECTOR(31 downto 0);
		aluout, writedata:   buffer STD_LOGIC_VECTOR(31 downto 0);
		readdata:            in  STD_LOGIC_VECTOR(31 downto 0));
	--PCSrcD  -> pcsrc
	--AlusrcD -> alusrc
	--RegDst  -> regdst
end;

architecture struct of datapath is
	component alu
		port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
			alucontrol: in  STD_LOGIC_VECTOR(2 downto 0);
			result:     buffer STD_LOGIC_VECTOR(31 downto 0);
			zero:       out STD_LOGIC);
	end component;
	component regfile
		port(clk:           in  STD_LOGIC;
			we3:           in  STD_LOGIC;
			ra1, ra2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);
			wd3:           in  STD_LOGIC_VECTOR(31 downto 0);
			rd1, rd2:      out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component adder
		port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
			y:    out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component sl2
		port(a: in  STD_LOGIC_VECTOR(31 downto 0);
			y: out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component signext
		port(a: in  STD_LOGIC_VECTOR(15 downto 0);
			y: out STD_LOGIC_VECTOR(31 downto 0));
	end component;
	component flopr generic(width: integer);
		port(clk, reset: in  STD_LOGIC;
			d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
			q:          out STD_LOGIC_VECTOR(width-1 downto 0));
	end component;
	component mux2 generic(width: integer);
		port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
			s:      in  STD_LOGIC;
			y:      out STD_LOGIC_VECTOR(width-1 downto 0));
	end component;
	component mux4 generic(width: integer);
		port(d0, d1, d2, d3: in  STD_LOGIC_VECTOR(width-1 downto 0);
			s:      in  STD_LOGIC_VECTOR(1 downto 0);
			y:      out STD_LOGIC_VECTOR(width-1 downto 0));
	end component;
	component equal is 
		port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
			y:    out STD_LOGIC);
	end component;
	component registrador_n
		generic (
			constant N: integer := 16 );
		port (
			clock, clear, enable: in std_logic;
			D: in std_logic_vector(N-1 downto 0);
			Q: out std_logic_vector (N-1 downto 0) );
	end component;
	component hazard 
		port(WriteRegE, WriteRegM, WriteRegW:                      in STD_LOGIC_VECTOR(4 downto 0);
			RsD, RtD, RtE, RsE:                                    in STD_LOGIC_VECTOR(4 downto 0);
			MemtoRegE, MemtoRegM, RegWriteE, RegWriteM, RegWriteW: in STD_LOGIC;
			BranchD, Branch_neD:                                   in STD_LOGIC;
			ForwardAD, ForwardBD:                                  out STD_LOGIC;
			ForwardAE, ForwardBE:                                  out STD_LOGIC_VECTOR(1 downto 0);
			StallF, StallD, FlushE:                                out STD_LOGIC);
	end component;
	signal writereg                 : STD_LOGIC_VECTOR(4 downto 0);
	signal pcjump, pcnext,
		pcnextbr, pcplus4,
	pcbranch:                  STD_LOGIC_VECTOR(31 downto 0);
	signal signimm, signimmsh       : STD_LOGIC_VECTOR(31 downto 0);
	signal srca, srcb, result       : STD_LOGIC_VECTOR(31 downto 0);
	signal PCPlus4D, PCPlus4E       : STD_LOGIC_VECTOR(31 downto 0);
	signal PCBranchM                : STD_LOGIC_VECTOR(31 downto 0);
	signal SrcAE, SrcAE2, WriteDataE: STD_LOGIC_VECTOR(31 downto 0);
	signal WriteDataE2              : STD_LOGIC_VECTOR(31 downto 0);
	signal RtE, RdE, RsE            : STD_LOGIC_VECTOR(4  downto 0);
	signal SignimmE                 : STD_LOGIC_VECTOR(31 downto 0);
	signal WriteRegM                : STD_LOGIC_VECTOR(4  downto 0);
	signal WriteRegW                : STD_LOGIC_VECTOR(4  downto 0);
	signal WriteDataM,WriteDataD    : STD_LOGIC_VECTOR(31 downto 0);
	signal ALUOutE, ALUOutM, ALUOutW: STD_LOGIC_VECTOR(31 downto 0);
	signal ReadDataW                : STD_LOGIC_VECTOR(31 downto 0);
	signal s_PCNextBR               : STD_LOGIC_VECTOR(31 downto 0);
	signal ZeroE, ZeroM             : STD_LOGIC_VECTOR(0 downto 0);
	signal ForwardAE, ForwardBE     : STD_LOGIC_VECTOR(1 downto 0);
	signal StallF, s_StallD, FlushE : STD_LOGIC;
	signal ForwardAD, ForwardBD     : STD_LOGIC;
	signal CompareOne, CompareTwo   : STD_LOGIC_VECTOR(31 downto 0);
begin
	-- next PC logic
	pcjump <= pcplus4(31 downto 28) & instr(25 downto 0) & "00";
	pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc);
	pcadd1: adder port map(pc, X"00000004", pcplus4);
	immsh: sl2 port map(SignimmE, signimmsh);
	pcadd2: adder port map(PCPlus4E, signimmsh, pcbranch);					 
	pcbrmux: mux2 generic map(32) port map(pcplus4, PCBranchM,
		pcsrc, pcnextbr);									
	pcmux: mux2 generic map(32) port map(s_PCNextBR, pcjump, jump, pcnext);
	
	-- register file logic
	rf: regfile port map(clk, regwrite, instr(25 downto 21),
		instr(20 downto 16), WriteRegW, result, srca,
		WriteDataD);
	
	muxcompareOne: mux2 generic map(32) port map(srca, ALUOutM, ForwardAD, CompareOne);
	
	muxcompareTwo: mux2 generic map(32) port map(WriteDataD, ALUOutM, ForwardBD, CompareTwo);
	
	igual: equal port map(CompareOne, CompareTwo, equalD);
	
	wrmux: mux2 generic map(5) port map(RtE,
		RdE,
		regdst, writereg);
	
	resmux: mux2 generic map(32) port map(ALUOutW, ReadDataW,
		memtoreg, result);
	
	se: signext port map(instr(15 downto 0), signimm);
	
	-- ALU logic
	segundoMux4: mux4 generic map(32) port map(WriteDataE, result, ALUOutM, "00000000000000000000000000000000", ForwardBE, WriteDataE2);
	
	srcbmux: mux2 generic map(32) port map(WriteDataE2, SignimmE, alusrc,
		srcb);
	
	primeiroMux4: mux4 generic map(32) port map(SrcAE, result, ALUOutM, "00000000000000000000000000000000", ForwardAE, SrcAE2);
	
	mainalu: alu port map(SrcAE2, srcb, alucontrol, ALUOutE, ZeroE(0));
	
	-- Hazard Unit
	hazardUnit: hazard port map(writereg, WriteRegM, WriteRegW, instr(25 downto 21), instr(20 downto 16), RtE,
		RsE, memtoRegE, memtoRegM, regwriteE, regwriteM, regwrite, branchD, branch_notequalD,
		ForwardAD, ForwardBD, ForwardAE, ForwardBE, StallF, s_StallD, FlushE);
	
	-- Registrador do PC'
	regPC: registrador_n generic map (N=>32) port map (clk, '0', not(StallF), pcnextbr, s_PCNextBR);
	
	-- Registrador D
	regD: registrador_n generic map (N => 32) port map(clk, pcsrc, not(s_StallD), pcplus4, PCPlus4D);
	
	-- Registrador E
	regE_PCPlus4D  : registrador_n generic map (N => 32) port map(clk, FlushE, '1', PCPlus4D, PCPlus4E);
	regE_SrcAE     : registrador_n generic map (N => 32) port map(clk, FlushE, '1', srca, SrcAE);
	regE_WriteDataE: registrador_n generic map (N => 32) port map(clk, FlushE, '1', WriteDataD, WriteDataE);
	regE_RtE       : registrador_n generic map (N =>  5) port map(clk, FlushE, '1', instr(20 downto 16), RtE);
	regE_RdE       : registrador_n generic map (N =>  5) port map(clk, FlushE, '1', instr(15 downto 11), RdE);
	regE_RsE       : registrador_n generic map (N =>  5) port map(clk, FlushE, '1', instr(25 downto 21), RsE);
	regE_SignimmE  : registrador_n generic map (N => 32) port map(clk, FlushE, '1', signimm, SignimmE);
	
	-- Registrador M
	regM_PCBranchM : registrador_n generic map (N => 32) port map(clk, '0', '1', pcbranch, PCBranchM);
	regM_WriteRegM : registrador_n generic map (N =>  5) port map(clk, '0', '1', writereg, WriteRegM);
	regM_WriteDataM: registrador_n generic map (N => 32) port map(clk, '0', '1', WriteDataE2, WriteDataM);
	regM_ALUOutM   : registrador_n generic map (N => 32) port map(clk, '0', '1', ALUOutE, ALUOutM);
	regM_ZeroM     : registrador_n generic map (N =>  1) port map(clk, '0', '1', ZeroE, ZeroM);
	
	-- Registrador W
	regW_WriteRegW : registrador_n generic map (N =>  5) port map(clk, '0', '1', WriteRegM, WriteRegW);
	regW_ALUOutW   : registrador_n generic map (N => 32) port map(clk, '0', '1', ALUOutM, ALUOutW);
	regW_ReadDataW : registrador_n generic map (N => 32) port map(clk, '0', '1', readdata, ReadDataW);
	
	stallD <= s_StallD;
	writedata <= WriteDataM;
	aluout <= ALUOutM;
	
end;

-- Inicio Equal ###########################################################################
library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity equal is 
	port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
		y:    out STD_LOGIC);
end;

architecture behave of equal is
begin
	y <= '1' when a = b else '0';
end;
-- ######################################################################################

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity regfile is -- three-port register file
	port(clk:          in  STD_LOGIC;
		we3:           in  STD_LOGIC;
		ra1, ra2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);
		wd3:           in  STD_LOGIC_VECTOR(31 downto 0);
		rd1, rd2:      out STD_LOGIC_VECTOR(31 downto 0));
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
			if we3 = '1' then mem(to_integer(wa3)) <= wd3;
			end if;
		end if;
	end process;
	process(all) begin
		if (to_integer(ra1) = 0) then rd1 <= X"00000000"; -- register 0 holds 0
		else rd1 <= mem(to_integer(ra1));
		end if;
		if (to_integer(ra2) = 0) then rd2 <= X"00000000";
		else rd2 <= mem(to_integer(ra2));
		end if;
	end process;
end;

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


library IEEE; use IEEE.STD_LOGIC_1164.all;

entity sl2 is -- shift left by 2
	port(a: in  STD_LOGIC_VECTOR(31 downto 0);
		y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of sl2 is
begin
	y <= a(29 downto 0) & "00";
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity signext is -- sign extender
	port(a: in  STD_LOGIC_VECTOR(15 downto 0);
		y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of signext is
begin
	y <= X"ffff" & a when a(15) else X"0000" & a;
end;

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

-- Mux2
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

-- Mux4
library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mux4 is 
	generic(width: integer);
	port(d0, d1, d2, d3: in  STD_LOGIC_VECTOR(width-1 downto 0);
		s:      in  STD_LOGIC_VECTOR(1 downto 0);
		y:      out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux4 is
begin
	y <= d0 when (s = "00") else
	d1 when (s = "01") else
	d2 when (s = "10") else
	d3 when (s = "11") else
	(others => '1');
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity alu is
	port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
		alucontrol: in  STD_LOGIC_VECTOR(2 downto 0);
		result:     buffer STD_LOGIC_VECTOR(31 downto 0);
		zero:       out STD_LOGIC);
end;

architecture behave of alu is
	signal condinvb, sum: STD_LOGIC_VECTOR(31 downto 0);
begin
	condinvb <= not b when alucontrol(2) else b;
	sum <= a + condinvb + alucontrol(2);
	
	process(all) begin
		case alucontrol(1 downto 0) is
			when "00"   => result <= a and b;
			when "01"   => result <= a or b;
			when "10"   => result <= sum;
			when "11"   => result <= (0 => sum(31), others => '0');
			when others => result <= (others => 'X');
		end case;
	end process;
	
	zero <= '1' when result = X"00000000" else '0';
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity hazard is
	port(WriteRegE, WriteRegM, WriteRegW:                       in STD_LOGIC_VECTOR(4 downto 0);
		RsD, RtD, RtE, RsE:                                    in STD_LOGIC_VECTOR(4 downto 0);
		MemtoRegE, MemtoRegM, RegWriteE, RegWriteM, RegWriteW: in STD_LOGIC;
		BranchD, Branch_neD:                                   in STD_LOGIC;
		ForwardAD, ForwardBD:                                  out STD_LOGIC;
		ForwardAE, ForwardBE:                                  out STD_LOGIC_VECTOR(1 downto 0);
		StallF, StallD, FlushE:                                out STD_LOGIC);
end;

architecture behave of hazard is
	signal lwstall, branchstall: STD_LOGIC;
begin
	
	-- Logic for ForwardAE (pg. 589)
	process(all) begin
		if ((RsE /= "00000") and (RsE = WriteRegM) and RegWriteM = '1') then ForwardAE <= "10";
		elsif ((RsE /= "00000") and (RsE = WriteRegW) and RegWriteW = '1') then ForwardAE <= "01";
		else ForwardAE <= "00";
		end if;
	end process;
	
	-- Logic for ForwardBE (pg. 589)
	process(all) begin
		if ((RtE /= "00000") and (RtE = WriteRegM) and RegWriteM = '1') then ForwardBE <= "10";
		elsif ((RtE /= "00000") and (RtE = WriteRegW) and RegWriteW = '1') then ForwardBE <= "01";
		else ForwardBE <= "00";
		end if;
	end process;
	
	-- Logic for Stalls (pg. 597)
	lwstall <= '1' when (((RsD = RtE) or (RtD = RtE)) and (MemtoRegE = '1')) else '0';
	branchstall <= '1' when (((BranchD = '1' or Branch_neD = '1') and (RegWriteE = '1') and (WriteRegE = RsD or WriteRegE = RtD)) or ((BranchD = '1' or Branch_neD = '1') and (MemtoRegM = '1') and (WriteRegM = RsD or WriteRegM = RtD))) else '0';
	StallD <= lwstall or branchstall;
	StallF <= lwstall or branchstall;
	FlushE <= lwstall or branchstall;

	-- Logic for ForwardAD and Forward BD
	ForwardAD <= '1' when ((RsD /= "00000") and (RsD = WriteRegM) and (RegWriteM = '1')) else '0';
	ForwardBD <= '1' when ((RtD /= "00000") and (RtD = WriteRegM) and (RegWriteM = '1')) else '0';
end;