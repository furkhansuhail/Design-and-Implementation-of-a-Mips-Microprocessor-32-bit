
module PC(pc,in,clk);
input [31:0] in;  
input clk;
output [31:0] pc;
reg [31:0] pc;
initial begin
	pc=32'b00000000000000000000000000000000;
end

always @(negedge clk)
begin
	pc = in;
end

endmodule


module PC_ALU(newpc,pc,extendaddr,chksignal);
input [31:0] pc;
input [31:0] extendaddr;
input chksignal;
output [31:0] newpc;
assign newpc = (chksignal)?(pc+extendaddr):(pc+1);
endmodule

module Splitter(inst,opcode,rs,rt,rd,funct,addr);
input [31:0] inst;
output [5:0] opcode;
output [4:0] rs;
output [4:0] rt;
output [4:0] rd;
output [5:0] funct;
output [15:0] addr;
reg [4:0] rs;
reg [4:0] rt;
reg [4:0] rd;
reg [5:0] funct;
reg [15:0] addr;

assign opcode = inst[31:26];

always @(inst)
begin
  
	if(inst[31:26]==6'b000000)
	begin
	rs = inst[25:21];
	rt = inst[20:16];
	rd = inst[15:11];
	funct = inst[5:0];
	end
	else
	begin
	rs = inst[25:21];
	rt = inst[20:16];
	addr = inst[15:0];
	end

end

endmodule


module SignExtend(in,out);
input [15:0] in;
output [31:0] out;

  //assign out[31:16] = in[15];
	assign out[31:16] = 1'b0;
	assign out[15:0] = in[15:0];

endmodule

//module SignExtendShift(in,out);
//input [15:0] in;
//output [31:0] out;

//        assign out[31:16] = in[15];
//        assign out[15:0] = in[15:0];

//	assign out = out << 2;


//endmodule

module RegisterFile(rw,addr1,addr2,out1,out2,addr3,data3,clk);

input rw;  	//A signal that decides whether we write into file (if 1 -> write)
input [4:0] addr1;	//In case of read, the address of first register
input [4:0] addr2;	//In case of read, the address of second register
output [31:0] out1;	//In case of read, the value of the first register
output [31:0] out2;	//In case of read, the value of the second register
input [4:0] addr3;	//In case of write, the address of the register in which to write
input [31:0] data3;	//In case of write, the value to be written
//reg [31:0] out1;
//reg [31:0] out2;

input clk;

reg [31:0] regmem [31:0];

initial begin
	regmem[0] = 32'b00000000000000000000000000000010;		//Register File initialized with random values
    regmem[1] = 32'b00000000000000000000000000000001;
    regmem[2] = 32'b00000000000000000000000000001000;
    regmem[3] = 32'b00000000000000000000000000000110;
    regmem[4] = 32'b00000000000000000000000001000000;
    regmem[5] = 32'b00000000000000000000000000100000;
    regmem[6] = 32'b00000000000000000000000000010000;
    regmem[7] = 32'b00000000000000000000001000000000;
    regmem[8] = 32'b00000000000000000000000010000000;
    regmem[9] = 32'b00000000000000000000000000100000;
    regmem[10] = 32'b00000000000000000000010000000000;
    regmem[11] = 32'b00000000000000000000000100000000;
    regmem[12] = 32'b00000000000000000000000000010010;
    regmem[13] = 32'b00000000000000000000000010100000;
    regmem[14] = 32'b00000000000000000000000000111000;
    regmem[15] = 32'b00000000000000000000011001000000;
    regmem[16] = 32'b00000000000000000000000000100000;
    regmem[17] = 32'b00000000000000000000000000010000;
    regmem[18] = 32'b00000000000000000000000010100000;
    regmem[19] = 32'b00000000000000000000000001000000;
    regmem[20] = 32'b00000000000000000000000101000000;
    regmem[21] = 32'b00000000000000000010000000000000;
    regmem[22] = 32'b00000000000000000001000000000000;
    regmem[23] = 32'b00000000000000000000100000000000;
    regmem[24] = 32'b00000000000000001000000100000000;
    regmem[25] = 32'b00000000000000001100100000000000;
    regmem[26] = 32'b00000000000000000000000100000000;
    regmem[27] = 32'b00000000000000000100100000000000;
    regmem[28] = 32'b00000000000000000000100001000000;
    regmem[29] = 32'b00000000000000000000000100010000;
    regmem[30] = 32'b00000000000010100010000000000000;
    regmem[31] = 32'b00000000000000010000100101100000;
	end

		assign out1 = regmem[addr1];
		assign out2 = regmem[addr2];

	//always@(rw)
	always@ (negedge clk)
	begin
		if(rw==1'b1)
		begin
		 regmem[addr3] = data3;
		end
	end

endmodule



module InstructionMemory(inst,pc,clk);
input clk;
input [31:0] pc;
output [31:0] inst;
reg [31:0] inst;

reg [31:0] memdata [127:0];

initial
        begin
        //128 instructions to be added here, each of 32 bits, but for sake of simplicity, only added 7 instructions

        memdata[0] = 32'b00000000000000010001000000100010;
        memdata[1] = 32'b00000000001000100001100000100100;
        memdata[2] = 32'b00000000010000010011000000100101;
        memdata[3] = 32'b00000001100010010101000000100000;
        memdata[4] = 32'b10001100000000010001000000100000;
        memdata[5] = 32'b10101100000000010001000000100000;
        memdata[6] = 32'b00001000000000010001000000100000;
        memdata[7] = 32'b00000000000000010001000000100000;
        memdata[8] = 32'b00000000000000010001000000100000;
        memdata[9] = 32'b00000000000000010001000000100000;
		end

always @(posedge clk)
        begin
                inst = memdata[pc];
        end

endmodule

module AND(in1,in2,out);                //this module does a 32-bit AND operation
input [31:0] in1;
input [31:0] in2;
output [31:0] out;

and A0(out[0],in1[0],in2[0]);
and A1(out[1],in1[1],in2[1]);
and A2(out[2],in1[2],in2[2]);
and A3(out[3],in1[3],in2[3]);
and A4(out[4],in1[4],in2[4]);
and A5(out[5],in1[5],in2[5]);
and A6(out[6],in1[6],in2[6]);
and A7(out[7],in1[7],in2[7]);
and A8(out[8],in1[8],in2[8]);
and A9(out[9],in1[9],in2[9]);
and A10(out[10],in1[10],in2[10]);
and A11(out[11],in1[11],in2[11]);
and A12(out[12],in1[12],in2[12]);
and A13(out[13],in1[13],in2[13]);
and A14(out[14],in1[14],in2[14]);
and A15(out[15],in1[15],in2[15]);
and A16(out[16],in1[16],in2[16]);
and A17(out[17],in1[17],in2[17]);
and A18(out[18],in1[18],in2[18]);
and A19(out[19],in1[19],in2[19]);
and A20(out[20],in1[20],in2[20]);
and A21(out[21],in1[21],in2[21]);
and A22(out[22],in1[22],in2[22]);
and A23(out[23],in1[23],in2[23]);
and A24(out[24],in1[24],in2[24]);
and A25(out[25],in1[25],in2[25]);
and A26(out[26],in1[26],in2[26]);
and A27(out[27],in1[27],in2[27]);
and A28(out[28],in1[28],in2[28]);
and A29(out[29],in1[29],in2[29]);
and A30(out[30],in1[30],in2[30]);
and A31(out[31],in1[31],in2[31]);
endmodule

module OR(in1,in2,out);                //this module does a 32-bit OR operation
input [31:0] in1;
input [31:0] in2;
output [31:0] out;

or o0(out[0],in1[0],in2[0]);
or o1(out[1],in1[1],in2[1]);
or o2(out[2],in1[2],in2[2]);
or o3(out[3],in1[3],in2[3]);
or o4(out[4],in1[4],in2[4]);
or o5(out[5],in1[5],in2[5]);
or o6(out[6],in1[6],in2[6]);
or o7(out[7],in1[7],in2[7]);
or o8(out[8],in1[8],in2[8]);
or o9(out[9],in1[9],in2[9]);
or o10(out[10],in1[10],in2[10]);
or o11(out[11],in1[11],in2[11]);
or o12(out[12],in1[12],in2[12]);
or o13(out[13],in1[13],in2[13]);
or o14(out[14],in1[14],in2[14]);
or o15(out[15],in1[15],in2[15]);
or o16(out[16],in1[16],in2[16]);
or o17(out[17],in1[17],in2[17]);
or o18(out[18],in1[18],in2[18]);
or o19(out[19],in1[19],in2[19]);
or o20(out[20],in1[20],in2[20]);
or o21(out[21],in1[21],in2[21]);
or o22(out[22],in1[22],in2[22]);
or o23(out[23],in1[23],in2[23]);
or o24(out[24],in1[24],in2[24]);
or o25(out[25],in1[25],in2[25]);
or o26(out[26],in1[26],in2[26]);
or o27(out[27],in1[27],in2[27]);
or o28(out[28],in1[28],in2[28]);
or o29(out[29],in1[29],in2[29]);
or o30(out[30],in1[30],in2[30]);
or o31(out[31],in1[31],in2[31]);

endmodule

module DataMemory(opcode,Rt,addr,clk,out);
input [5:0] opcode;
input [31:0] addr;  //Rather than taking Rs and num as inputs, just take their sum (here 'addr') from caller space and send to DataMemory
input [31:0] Rt;	//Rt is the value of the register itself; not the address of the register in the register file
input clk;
output [31:0] out;
//reg [31:0] out;


reg [31:0] memdata [255:0];

initial begin

        //256 data to be added here, each being a 32-bit '0';for simplicity, added just 20 data

		memdata[0] = 32'b00000000000000000000000000000000;
        memdata[1] = 32'b00000000000000000000000000000000;
        memdata[2] = 32'b00000000000000000000000000000000;
        memdata[3] = 32'b00000000000000000000000000000000;
        memdata[4] = 32'b00000000000000000000000000000000;
        memdata[5] = 32'b00000000000000000000000000000000;
        memdata[6] = 32'b00000000000000000000000000000000;
        memdata[7] = 32'b00000000000000000000000000000000;
        memdata[8] = 32'b00000000000000000000000000000000;
        memdata[9] = 32'b00000000000000000000000000000000;
        memdata[10] = 32'b00000000000000000000000000000000;
        memdata[11] = 32'b00000000000000000000000000000000;
        memdata[12] = 32'b00000000000000000000000000000000;
        memdata[13] = 32'b00000000000000000000000000000000;
        memdata[14] = 32'b00000000000000000000000000000000;
        memdata[15] = 32'b00000000000000000000000000000000;
        memdata[16] = 32'b00000000000000000000000000000000;
        memdata[17] = 32'b00000000000000000000000000000000;
        memdata[18] = 32'b00000000000000000000000000000000;
        memdata[19] = 32'b00000000000000000000000000000000;

        end

	assign out = memdata[addr];


always @(negedge clk)
begin
	//if(opcode==6'b100011)
	//begin
	//out = memdata[addr];	//In caller space, 'out' needs to be stored in the register file at the address of Rt given by the instruction
	//end
	
	if(opcode==6'b101011)
	begin
	memdata[addr] = Rt;
	end

end
endmodule

module clkGen(out);
output out;
reg    out;
//Tested and fully functional
initial begin
        out = 1'b0;
end
always
begin
        out = #1 ~out;
end
endmodule

module ALU(opcode,funct,in1,in2,result,rw,clk);
input clk;
input [5:0] opcode;
input [5:0] funct;
input [31:0] in1;
input [31:0] in2;
output [31:0] result;
reg [31:0] result;
output rw;
reg rw;  	//A signal that decides whether we write into file (if 1 -> write)

wire [31:0] sum;
wire [31:0] diff;
wire [31:0] product;
wire [31:0] sum_or;

thirtytwobitadder ADD(in1,in2,carryout,sum,1'b0);
thirtytwobitsubtractor SUBTRACT(in1,in2,carry,diff,1'b0);
AND prod(in1,in2,product);
OR orop(in1,in2,sum_or);

always @(*)
begin

        if(opcode==6'b000000)
        begin
                if(funct==6'b100000)
                begin
                        rw=1'b0;
                        result=sum;
                        rw=1'b1;
                end
                if(funct==6'b100010)
                begin
                        rw=1'b0;
                        result=diff;
                        rw=1'b1;
                end
                if(funct==6'b100100)
                begin
                        rw=1'b0;
                        result=product;
                        rw=1'b1;
                end
                if(funct==6'b100101)
                begin
                        rw=1'b0;
                        result=sum_or;
                        rw=1'b1;
                end
        end

        if(opcode==6'b100011)
        begin
        rw=1'b0;
        result=sum;
        rw=1'b1;
        end

        if(opcode==6'b101011)
        begin
        rw=1'b0;
        result=sum;
        end

        if(opcode==6'b000100)
        begin
        rw=1'b0;

                if(diff==32'b00000000000000000000000000000000)
                begin
                result=diff;
                end

        end

end

endmodule


module onebitadder(x,y,sum,carryout,carryin);
input x,y,carryin;
output sum,carryout;

        //sum = x XOR y XOR z, where z is carry in
        //carryout = x XOR y multiplied by z plus xy

xor XOR1(p,x,y);
xor XOR2(sum,p,carryin);
        //At this stage, we have sum with us

and ANDp(q,p,carryin);
and ANDxy(r,x,y);
or ORing(carryout,q,r);
        //At this stage, we have carryout also with us

endmodule

module thirtytwobitadder(a,b,carryout,s,carryin);

input carryin;
input [31:0] a;
input [31:0] b;
output carryout;
output [31:0] s;

onebitadder adder0(a[0],b[0],s[0],c1,carryin);
onebitadder adder1(a[1],b[1],s[1],c2,c1);
onebitadder adder2(a[2],b[2],s[2],c3,c2);
onebitadder adder3(a[3],b[3],s[3],c4,c3);
onebitadder adder4(a[4],b[4],s[4],c5,c4);
onebitadder adder5(a[5],b[5],s[5],c6,c5);
onebitadder adder6(a[6],b[6],s[6],c7,c6);
onebitadder adder7(a[7],b[7],s[7],c8,c7);
onebitadder adder8(a[8],b[8],s[8],c9,c8);
onebitadder adder9(a[9],b[9],s[9],c10,c9);
onebitadder adder10(a[10],b[10],s[10],c11,c10);
onebitadder adder11(a[11],b[11],s[11],c12,c11);
onebitadder adder12(a[12],b[12],s[12],c13,c12);
onebitadder adder13(a[13],b[13],s[13],c14,c13);
onebitadder adder14(a[14],b[14],s[14],c15,c14);
onebitadder adder15(a[15],b[15],s[15],c16,c15);
onebitadder adder16(a[16],b[16],s[16],c17,c16);
onebitadder adder17(a[17],b[17],s[17],c18,c17);
onebitadder adder18(a[18],b[18],s[18],c19,c18);
onebitadder adder19(a[19],b[19],s[19],c20,c19);
onebitadder adder20(a[20],b[20],s[20],c21,c20);
onebitadder adder21(a[21],b[21],s[21],c22,c21);
onebitadder adder22(a[22],b[22],s[22],c23,c22);
onebitadder adder23(a[23],b[23],s[23],c24,c23);
onebitadder adder24(a[24],b[24],s[24],c25,c24);
onebitadder adder25(a[25],b[25],s[25],c26,c25);
onebitadder adder26(a[26],b[26],s[26],c27,c26);
onebitadder adder27(a[27],b[27],s[27],c28,c27);
onebitadder adder28(a[28],b[28],s[28],c29,c28);
onebitadder adder29(a[29],b[29],s[29],c30,c29);
onebitadder adder30(a[30],b[30],s[30],c31,c30);
onebitadder adder31(a[31],b[31],s[31],carryout,c31);

endmodule

/* Logic for subtraction: Suppose you want to perform 8 - 4
        first take 2's complement of 4 and add it to 8
        to take 2's complement of 4, first take 1's complement and add 1 to it
        to take 1's complement, perform XOR operation on the bits with the bit '1'
*/

module thirtytwobitsubtractor(a,b,carryout,s,carryin);

        //We perform a - b here
input [31:0] a;
input [31:0] b;
output [31:0] s;
input carryin;
output carryout;
wire [31:0] m;

xor X0(m[0],b[0],1);
xor X1(m[1],b[1],1);
xor X2(m[2],b[2],1);
xor X3(m[3],b[3],1);
xor X4(m[4],b[4],1);
xor X5(m[5],b[5],1);
xor X6(m[6],b[6],1);
xor X7(m[7],b[7],1);
xor X8(m[8],b[8],1);
xor X9(m[9],b[9],1);
xor X10(m[10],b[10],1);
xor X11(m[11],b[11],1);
xor X12(m[12],b[12],1);
xor X13(m[13],b[13],1);
xor X14(m[14],b[14],1);
xor X15(m[15],b[15],1);
xor X16(m[16],b[16],1);
xor X17(m[17],b[17],1);
xor X18(m[18],b[18],1);
xor X19(m[19],b[19],1);
xor X20(m[20],b[20],1);
xor X21(m[21],b[21],1);
xor X22(m[22],b[22],1);
xor X23(m[23],b[23],1);
xor X24(m[24],b[24],1);
xor X25(m[25],b[25],1);
xor X26(m[26],b[26],1);
xor X27(m[27],b[27],1);
xor X28(m[28],b[28],1);
xor X29(m[29],b[29],1);
xor X30(m[30],b[30],1);
xor X31(m[31],b[31],1);


thirtytwobitadder adding(a,m,carryout,s,1);

endmodule
