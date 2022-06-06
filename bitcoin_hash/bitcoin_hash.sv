`include "simplified_sha256.sv"

module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

enum logic [ 3:0] {IDLE, REST, READ, START1, BUFFER1, START2, BUFFER2, START3, BUFFER3, START4, BUFFER4, START5, BUFFER5, WRITE}state;
logic [31:0] w0[16], w1[16], w2[16],w3[16],w4[16],w5[16],w6[16],w7[16];
logic [31:0] message[32];
logic [31:0] h[8], ho[8], ho20[8], ho21[8], ho22[8], ho23[8], ho24[8], ho25[8], ho26[8], ho27[8];
logic [31:0] ho30[8], ho31[8], ho32[8], ho33[8], ho34[8], ho35[8], ho36[8], ho37[8], ho38[8];
logic [31:0] h0[16];
logic [ 6:0] num;
logic [ 4:0] offset; 
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic start_1, start_2, start_3;
logic done_0, done_20, done_30, done_1, done_2, done_3, done_4, done_5, done_6, done_7,done_31,done_32,done_33,done_34,done_35,done_36,done_37;

assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;

logic [31:0] k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
}; 
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
  end 
  else case (state)
   IDLE: begin 
       if(start) begin
			h[0] <= 32'h6a09e667;
			h[1] <= 32'hbb67ae85;
			h[2] <= 32'h3c6ef372;
			h[3] <= 32'ha54ff53a;
			h[4] <= 32'h510e527f;
			h[5] <= 32'h9b05688c;
			h[6] <= 32'h1f83d9ab;
			h[7] <= 32'h5be0cd19; 	
			cur_addr <= message_addr;
			cur_we <= 1'b0;
			offset <= 5'b0;
			num <= 7'b0;
			
			start_1 <= 0;	
			start_2 <= 0;	
			
			state <= REST;
       end
    end

	 REST:begin
		state <= READ;
    end 
	 
	  READ: begin
			if(offset < 19) 
				begin
					message[offset] <= mem_read_data;
					offset <= offset + 1;
					state <= REST;
				end
			else begin
				for(int n=0; n<16; n++) w0[n] <= message[n];
				message[20] <= 32'h80000000;
				message[31] <= 32'd640;
				for(int n = 21; n<31; n++) message[n] <= 32'h0;
				offset <= 0;
				state <=START1;	
			end
		end			
		
		
    START1: begin
				start_1 <= 1;
				num <= num + 1;
				state <= BUFFER1;
    end

	 BUFFER1: begin
		if(num < 3) begin
			num <= num + 1;
			state <= BUFFER1;
		end
		else begin
		start_1 <= 0;
		if(done_0) begin
			num <= 0;
			for(int n=0;n<3;n++) w0[n] <= message[n+16];
			for(int n=0;n<3;n++) w1[n] <= message[n+16];
			for(int n=0;n<3;n++) w2[n] <= message[n+16];
			for(int n=0;n<3;n++) w3[n] <= message[n+16];
			for(int n=0;n<3;n++) w4[n] <= message[n+16];
			for(int n=0;n<3;n++) w5[n] <= message[n+16];
			for(int n=0;n<3;n++) w6[n] <= message[n+16];
			for(int n=0;n<3;n++) w7[n] <= message[n+16];
			w0[3] <= 32'd0;
			w1[3] <= 32'd1;
			w2[3] <= 32'd2;
			w3[3] <= 32'd3;
			w4[3] <= 32'd4;
			w5[3] <= 32'd5;
			w6[3] <= 32'd6;
			w7[3] <= 32'd7;
			for(int n=4;n<16;n++) w0[n] <= message[n+16];
			for(int n=4;n<16;n++) w1[n] <= message[n+16];
			for(int n=4;n<16;n++) w2[n] <= message[n+16];
			for(int n=4;n<16;n++) w3[n] <= message[n+16];
			for(int n=4;n<16;n++) w4[n] <= message[n+16];
			for(int n=4;n<16;n++) w5[n] <= message[n+16];
			for(int n=4;n<16;n++) w6[n] <= message[n+16];
			for(int n=4;n<16;n++) w7[n] <= message[n+16];
			state <= START2;
		end
		else begin
			state <= BUFFER1;
		end
		end
	end
	
	START2: begin
			start_2 <= 1;
			num <= num + 1;
			state <= BUFFER2;
    end

	 BUFFER2: begin
		if(num < 3) begin
			num <= num + 1;
			state <= BUFFER2;
		end
		else begin
		start_2 <= 0;
		if(done_20) begin
			num <= 0;
			for(int n=0;n<8;n++) w0[n] <= ho20[n];
			for(int n=0;n<8;n++) w1[n] <= ho21[n];
			for(int n=0;n<8;n++) w2[n] <= ho22[n];
			for(int n=0;n<8;n++) w3[n] <= ho23[n];
			for(int n=0;n<8;n++) w4[n] <= ho24[n];
			for(int n=0;n<8;n++) w5[n] <= ho25[n];
			for(int n=0;n<8;n++) w6[n] <= ho26[n];
			for(int n=0;n<8;n++) w7[n] <= ho27[n];
			w0[8] <= 32'h80000000;
			w1[8] <= 32'h80000000;
			w2[8] <= 32'h80000000;
			w3[8] <= 32'h80000000;
			w4[8] <= 32'h80000000;
			w5[8] <= 32'h80000000;
			w6[8] <= 32'h80000000;
			w7[8] <= 32'h80000000;
			for(int n=9; n<15; n++) begin
				w0[n] <= 32'h0;
				w1[n] <= 32'h0;
				w2[n] <= 32'h0;
				w3[n] <= 32'h0;
				w4[n] <= 32'h0;
				w5[n] <= 32'h0;
				w6[n] <= 32'h0;
				w7[n] <= 32'h0;
			end
			w0[15] <= 32'd256;
			w1[15] <= 32'd256;
			w2[15] <= 32'd256;
			w3[15] <= 32'd256;
			w4[15] <= 32'd256;
			w5[15] <= 32'd256;
			w6[15] <= 32'd256;
			w7[15] <= 32'd256;
			state <= START3;
		end
		else begin
			state <= BUFFER2;
		end
		end
	end
	
	START3: begin
			start_3 <= 1;
			num <= num + 1;
			state <= BUFFER3;
    end

	 BUFFER3: begin
		if(num < 3) begin
			num <= num + 1;
			state <= BUFFER3;
		end
		else begin
		start_3 <= 0;
		if(done_30) begin
			num <= 0;
			for(int n=0;n<3;n++) w0[n] <= message[n+16];
			for(int n=0;n<3;n++) w1[n] <= message[n+16];
			for(int n=0;n<3;n++) w2[n] <= message[n+16];
			for(int n=0;n<3;n++) w3[n] <= message[n+16];
			for(int n=0;n<3;n++) w4[n] <= message[n+16];
			for(int n=0;n<3;n++) w5[n] <= message[n+16];
			for(int n=0;n<3;n++) w6[n] <= message[n+16];
			for(int n=0;n<3;n++) w7[n] <= message[n+16];
			w0[3] <= 32'd8;
			w1[3] <= 32'd9;
			w2[3] <= 32'd10;
			w3[3] <= 32'd11;
			w4[3] <= 32'd12;
			w5[3] <= 32'd13;
			w6[3] <= 32'd14;
			w7[3] <= 32'd15;
			for(int n=4;n<16;n++) w0[n] <= message[n+16];
			for(int n=4;n<16;n++) w1[n] <= message[n+16];
			for(int n=4;n<16;n++) w2[n] <= message[n+16];
			for(int n=4;n<16;n++) w3[n] <= message[n+16];
			for(int n=4;n<16;n++) w4[n] <= message[n+16];
			for(int n=4;n<16;n++) w5[n] <= message[n+16];
			for(int n=4;n<16;n++) w6[n] <= message[n+16];
			for(int n=4;n<16;n++) w7[n] <= message[n+16];
			h0[0] <= ho30[0];
			h0[1] <= ho31[0];
			h0[2] <= ho32[0];
			h0[3] <= ho33[0];
			h0[4] <= ho34[0];
			h0[5] <= ho35[0];
			h0[6] <= ho36[0];
			h0[7] <= ho37[0];
			
			state <= START4;
		end
		else begin
			state <= BUFFER3;
		end
		end
	end
		
		START4: begin
			start_2 <= 1;
			num <= num + 1;
			state <= BUFFER4;
		end
		
		BUFFER4: begin
			if(num < 3) begin
				num <= num + 1;
				state <= BUFFER4;
			end
			else begin
				start_2 <= 0;
				if(done_20) begin
					num <= 0;
					for(int n=0;n<8;n++) w0[n] <= ho20[n];
					for(int n=0;n<8;n++) w1[n] <= ho21[n];
					for(int n=0;n<8;n++) w2[n] <= ho22[n];
					for(int n=0;n<8;n++) w3[n] <= ho23[n];
					for(int n=0;n<8;n++) w4[n] <= ho24[n];
					for(int n=0;n<8;n++) w5[n] <= ho25[n];
					for(int n=0;n<8;n++) w6[n] <= ho26[n];
					for(int n=0;n<8;n++) w7[n] <= ho27[n];
					w0[8] <= 32'h80000000;
					w1[8] <= 32'h80000000;
					w2[8] <= 32'h80000000;
					w3[8] <= 32'h80000000;
					w4[8] <= 32'h80000000;
					w5[8] <= 32'h80000000;
					w6[8] <= 32'h80000000;
					w7[8] <= 32'h80000000;
					for(int n=9; n<15; n++) begin
						w0[n] <= 32'h0;
						w1[n] <= 32'h0;
						w2[n] <= 32'h0;
						w3[n] <= 32'h0;
						w4[n] <= 32'h0;
						w5[n] <= 32'h0;
						w6[n] <= 32'h0;
						w7[n] <= 32'h0;
					end
					w0[15] <= 32'd256;
					w1[15] <= 32'd256;
					w2[15] <= 32'd256;
					w3[15] <= 32'd256;
					w4[15] <= 32'd256;
					w5[15] <= 32'd256;
					w6[15] <= 32'd256;
					w7[15] <= 32'd256;
					state <= START5;
				end
				else begin
					state <= BUFFER4;
				end
			end
		end
		
		START5: begin
			start_3 <= 1;
			num <= num + 1;
			state <= BUFFER5;
		end
		
		BUFFER5: begin
			if(num < 3) begin
				num <= num + 1;
				state <= BUFFER5;
			end
			else begin
				start_3 <= 0;
				if(done_30) begin
					num <= 0;
					cur_addr <= output_addr;
					cur_we <= 1'b1;
					h0[8] <= ho30[0];
					h0[9] <= ho31[0];
					h0[10] <= ho32[0];
					h0[11] <= ho33[0];
					h0[12] <= ho34[0];
					h0[13] <= ho35[0];
					h0[14] <= ho36[0];
					h0[15] <= ho37[0];
			
					cur_write_data <= h0[0];
					state <= WRITE;
				end
				else begin
					state <= BUFFER5;
				end
			end
		end
					
		
	 WRITE: begin
		if(offset < 16)begin
			cur_write_data <= h0[offset+1];
			offset <= offset + 1;
			state <= WRITE;
		end
		else begin
		state <= IDLE;
		end
    end
   endcase
  end
	
assign done = (state == IDLE);	

simplified_sha256 sha1 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_1),
	.message(w0),
	.in(h),
	.k(k),
	.done(done_0),
	.sha256(ho)
	);

	simplified_sha256 sha20 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w0),
	.in(ho),
	.k(k),
	.done(done_20),
	.sha256(ho20)
	);
	
	simplified_sha256 sha21 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w1),
	.in(ho),
	.k(k),
	.done(done_1),
	.sha256(ho21)
	);
	
	simplified_sha256 sha22 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w2),
	.in(ho),
	.k(k),
	.done(done_2),
	.sha256(ho22)
	);
	
	simplified_sha256 sha23 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w3),
	.in(ho),
	.k(k),
	.done(done_3),
	.sha256(ho23)
	);
	
	simplified_sha256 sha24 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w4),
	.in(ho),
	.k(k),
	.done(done_4),
	.sha256(ho24)
	);
	
	simplified_sha256 sha25 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w5),
	.in(ho),
	.k(k),
	.done(done_5),
	.sha256(ho25)
	);
	
	simplified_sha256 sha26 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w6),
	.in(ho),
	.k(k),
	.done(done_6),
	.sha256(ho26)
	);
	
	simplified_sha256 sha27 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w7),
	.in(ho),
	.k(k),
	.done(done_7),
	.sha256(ho27)
	);
	
	simplified_sha256 sha30 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w0),
	.in(h),
	.k(k),
	.done(done_30),
	.sha256(ho30)
	);
	
	simplified_sha256 sha31 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w1),
	.in(h),
	.k(k),
	.done(done_31),
	.sha256(ho31)
	);
	
	simplified_sha256 sha32 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w2),
	.in(h),
	.k(k),
	.done(done_32),
	.sha256(ho32)
	);
	
	simplified_sha256 sha33 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w3),
	.in(h),
	.k(k),
	.done(done_33),
	.sha256(ho33)
	);
	
	simplified_sha256 sha34 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w4),
	.in(h),
	.k(k),
	.done(done_34),
	.sha256(ho34)
	);
	
	simplified_sha256 sha35 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w5),
	.in(h),
	.k(k),
	.done(done_35),
	.sha256(ho35)
	);
	
	simplified_sha256 sha36 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w6),
	.in(h),
	.k(k),
	.done(done_36),
	.sha256(ho36)
	);
	
	simplified_sha256 sha37 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w7),
	.in(h),
	.k(k),
	.done(done_37),
	.sha256(ho37)
	);
	
	
endmodule