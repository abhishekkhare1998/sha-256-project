`include "bitcoin_sha256_single.sv"

module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);


enum logic [ 3:0] {IDLE, DELAY,READ_ONE,DELAY2,READ_TWO,PHASE1, BUFFER1, PHASE2, BUFFER2, PHASE3, BUFFER3, PHASE2_FINAL, BUFFER4, PHASE3_FINAL, BUFFER5, WRITE}state;
logic [31:0] w0[16], w1[16], w2[16],w3[16],w4[16],w5[16],w6[16],w7[16];
logic [31:0] block1[16];
logic [31:0] block2[16];
logic [31:0] h[8], ho[8], phase2_0hash[8], phase2_1hash[8], phase2_2hash[8], phase2_3hash[8], phase2_4hash[8], phase2_5hash[8], phase2_6hash[8], phase2_7hash[8];
logic [31:0] phase3_0hash[8], phase3_1hash[8], phase3_2hash[8], phase3_3hash[8], phase3_4hash[8], phase3_5hash[8], phase3_6hash[8], phase3_7hash[8], phase3_8hash[8];
logic [31:0] output_hash[16];
logic [ 6:0] count;
logic [ 4:0] offset; 
logic        temp_we;
logic [15:0] ptr;
logic [31:0] cur_write_data;
logic start_1, start_2, start_3;
logic done_0, done_20, done_30, done_1, done_2, done_3, done_4, done_5, done_6, done_7,done_31,done_32,done_33,done_34,done_35,done_36,done_37;
//logic[15:0] initial_nonce;
assign mem_clk = clk;
assign mem_addr = ptr + offset;
assign mem_we = temp_we;
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
    temp_we <= 1'b0;
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
			ptr <= message_addr;
			temp_we <= 1'b0;
			offset <= 5'b0;
			count <= 7'b0;
			
			start_1 <= 0;	
			start_2 <= 0;	
			
			state <= DELAY;
       end
    end

	 DELAY:begin
		state <= READ_ONE;
    end 
	 
	  READ_ONE: begin//1
	  		 								 	

	  	//if(offset < 32)
	  	//begin//2
			if(offset < 16) 
				begin//3
						  		 								 	

					block1[offset] <= mem_read_data;
					offset <= offset + 1;
					state <= DELAY;
				end//2
			else begin//3
				//if(offset <)
					for(int n=0; n<16; n++) w0[n] <= block1[n];
					state<= DELAY2;
				/*for(int n=0; n<16; n++) w0[n] <= message[n];
				message[20] <= 32'h80000000;
				message[31] <= 32'd640;
				for(int n = 21; n<31; n++) message[n] <= 32'h0;
				offset <= 0;
				state <=START1;	*/
			end//1
		end//0
		DELAY2:begin
		state <= READ_TWO;
    end
		READ_TWO: begin
				  		 								 	

			if(offset < 20)
				begin		
				
				block2[offset-16] <= mem_read_data;
				offset<= offset +1;
				state<= DELAY2;
			end
			else
				begin
					block2[4] = 32'h80000000;
					block2[15] = 32'd640;
					for (int i = 21; i < 31; i++) begin
						block2[i-16] = 32'h0;
					end
					offset <= 0;
					state<= PHASE1;
				end
			end
		
    PHASE1: begin
    		 								 	
    		 								 	

				start_1 <= 1;
				count <= count + 1;
				state <= BUFFER1;
    end

	 BUFFER1: begin

		if(count < 3) begin
			count <= count + 1;
			state <= BUFFER1;
		end
		else begin
		start_1 <= 0;
		if(done_0) begin
			count <= 0;
			for(int k = 0 ;k<16;k++)
			begin
				if(k == 3)
					begin
				w0[3] <= 32'd0;
			  w1[3] <= 32'd1;
				w2[3] <= 32'd2;
				w3[3] <= 32'd3;
				w4[3] <= 32'd4;
				w5[3] <= 32'd5;
				w6[3] <= 32'd6;
				w7[3] <= 32'd7;
					end
					else
						begin
				w1[k] <= block2[k];
				w2[k] <= block2[k];
				w3[k] <= block2[k];
				w4[k] <= block2[k];
				w5[k] <= block2[k];
				w6[k] <= block2[k];
				w7[k] <= block2[k];
				w0[k] <= block2[k];
						end
				
				

			end
			 
			/*for(int n=0;n<3;n++) w0[n] <= block2[n];
			for(int n=0;n<3;n++) w1[n] <= block2[n];
			for(int n=0;n<3;n++) w2[n] <= block2[n];
			for(int n=0;n<3;n++) w3[n] <= block2[n];
			for(int n=0;n<3;n++) w4[n] <= block2[n];
			for(int n=0;n<3;n++) w5[n] <= block2[n];
			for(int n=0;n<3;n++) w6[n] <= block2[n];
			for(int n=0;n<3;n++) w7[n] <= block2[n];
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
			for(int n=4;n<16;n++) w7[n] <= message[n+16];*/
			state <= PHASE2;
		end
		else begin

			state <= BUFFER1;
		end
		end
	end
	
	PHASE2: begin
			start_2 <= 1;
			count <= count + 1;
			state <= BUFFER2;
    end

	 BUFFER2: begin

		if(count < 3) begin
			count <= count + 1;
			state <= BUFFER2;
		end
		else begin
		start_2 <= 0;
		if(done_20) begin
			count <= 0;
			for(int k = 0 ;k<16;k++)
			begin
				if(k == 8)
					begin
			w0[8] <= 32'h80000000;
			w1[8] <= 32'h80000000;
			w2[8] <= 32'h80000000;
			w3[8] <= 32'h80000000;
			w4[8] <= 32'h80000000;
			w5[8] <= 32'h80000000;
			w6[8] <= 32'h80000000;
			w7[8] <= 32'h80000000;
					end
					else
					if (k == 15) begin
				w0[15] <= 32'd256;
			w1[15] <= 32'd256;
			w2[15] <= 32'd256;
			w3[15] <= 32'd256;
			w4[15] <= 32'd256;
			w5[15] <= 32'd256;
			w6[15] <= 32'd256;
			w7[15] <= 32'd256;
						end
					else if (k > 8) begin
				w0[k] <= 32'h0;
				w1[k] <= 32'h0;
				w2[k] <= 32'h0;
				w3[k] <= 32'h0;
				w4[k] <= 32'h0;
				w5[k] <= 32'h0;
				w6[k] <= 32'h0;
				w7[k] <= 32'h0;
					end
				else
				begin
				w1[k] <= phase2_1hash[k];
				w2[k] <= phase2_2hash[k];
				w3[k] <= phase2_3hash[k];
				w4[k] <= phase2_4hash[k];
				w5[k] <= phase2_5hash[k];
				w6[k] <= phase2_6hash[k];
				w7[k] <= phase2_7hash[k];
				w0[k] <= phase2_0hash[k];
				
			end
		end
					state <= PHASE3;

		/*	for(int n=0;n<8;n++) w0[n] <= phase2_0hash[n];
			for(int n=0;n<8;n++) w1[n] <= phase2_1hash[n];
			for(int n=0;n<8;n++) w2[n] <= phase2_2hash[n];
			for(int n=0;n<8;n++) w3[n] <= phase2_3hash[n];
			for(int n=0;n<8;n++) w4[n] <= phase2_4hash[n];
			for(int n=0;n<8;n++) w5[n] <= phase2_5hash[n];
			for(int n=0;n<8;n++) w6[n] <= phase2_6hash[n];
			for(int n=0;n<8;n++) w7[n] <= phase2_7hash[n];*/
			/*w0[8] <= 32'h80000000;
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
			w7[15] <= 32'd256;*/
		end
		else begin
			state <= BUFFER2;
		end
		end
	end
	
	PHASE3: begin
			start_3 <= 1;
			count <= count + 1;
			state <= BUFFER3;
    end

	 BUFFER3: begin

		if(count < 3) begin
			count <= count + 1;
			state <= BUFFER3;
		end
		else begin
		start_3 <= 0;
		if(done_30) begin
			count <= 0;
			for(int k = 0 ;k<16;k++)
			begin
				if(k == 3)
					begin
						w0[3] <= 32'd8;
			  w1[3] <= 32'd9;
				w2[3] <= 32'd10;
				w3[3] <= 32'd11;
				w4[3] <= 32'd12;
				w5[3] <= 32'd13;
				w6[3] <= 32'd14;
				w7[3] <= 32'd15;
					end
					else
						begin
				w1[k] <= block2[k];
				w2[k] <= block2[k];
				w3[k] <= block2[k];
				w4[k] <= block2[k];
				w5[k] <= block2[k];
				w6[k] <= block2[k];
				w7[k] <= block2[k];
				w0[k] <= block2[k];
						end
				


			end
			output_hash[0] <= phase3_0hash[0];
			output_hash[1] <= phase3_1hash[0];
			output_hash[2] <= phase3_2hash[0];
			output_hash[3] <= phase3_3hash[0];
			output_hash[4] <= phase3_4hash[0];
			output_hash[5] <= phase3_5hash[0];
			output_hash[6] <= phase3_6hash[0];
			output_hash[7] <= phase3_7hash[0];
			/*for(int n=0;n<3;n++) w0[n] <= message[n+16];
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
			*/
			
			state <= PHASE2_FINAL;
		end
		else begin
			state <= BUFFER3;
		end
		end
	end
		
		PHASE2_FINAL: begin
			start_2 <= 1;
			count <= count + 1;
			state <= BUFFER4;
		end
		
		BUFFER4: begin

			if(count < 3) begin
				count <= count + 1;
				state <= BUFFER4;
			end
			else begin
				start_2 <= 0;
				if(done_20) begin
					count <= 0;
				for(int k = 0 ;k<16;k++)
			begin
				if(k == 8)
					begin
			w0[8] <= 32'h80000000;
			w1[8] <= 32'h80000000;
			w2[8] <= 32'h80000000;
			w3[8] <= 32'h80000000;
			w4[8] <= 32'h80000000;
			w5[8] <= 32'h80000000;
			w6[8] <= 32'h80000000;
			w7[8] <= 32'h80000000;
					end
					else
					if (k == 15) begin
				w0[15] <= 32'd256;
			w1[15] <= 32'd256;
			w2[15] <= 32'd256;
			w3[15] <= 32'd256;
			w4[15] <= 32'd256;
			w5[15] <= 32'd256;
			w6[15] <= 32'd256;
			w7[15] <= 32'd256;
						end
					else if (k > 8) begin
				w0[k] <= 32'h0;
				w1[k] <= 32'h0;
				w2[k] <= 32'h0;
				w3[k] <= 32'h0;
				w4[k] <= 32'h0;
				w5[k] <= 32'h0;
				w6[k] <= 32'h0;
				w7[k] <= 32'h0;
					end
				else
				begin
				w1[k] <= phase2_1hash[k];
				w2[k] <= phase2_2hash[k];
				w3[k] <= phase2_3hash[k];
				w4[k] <= phase2_4hash[k];
				w5[k] <= phase2_5hash[k];
				w6[k] <= phase2_6hash[k];
				w7[k] <= phase2_7hash[k];
				w0[k] <= phase2_0hash[k];
				
			end
		end
					state <= PHASE3_FINAL;
				end
				else begin
					state <= BUFFER4;
				end
			end
		end
		
		PHASE3_FINAL: begin

			start_3 <= 1;
			count <= count + 1;
			state <= BUFFER5;
		end
		
		BUFFER5: begin

			if(count < 3) begin
				count <= count + 1;
				state <= BUFFER5;
			end
			else begin
				start_3 <= 0;
				if(done_30) begin
					count <= 0;
					ptr <= output_addr;
					temp_we <= 1'b1;
					output_hash[8] <= phase3_0hash[0];
					output_hash[9] <= phase3_1hash[0];
					output_hash[10] <= phase3_2hash[0];
					output_hash[11] <= phase3_3hash[0];
					output_hash[12] <= phase3_4hash[0];
					output_hash[13] <= phase3_5hash[0];
					output_hash[14] <= phase3_6hash[0];
					output_hash[15] <= phase3_7hash[0];

					cur_write_data <= output_hash[0];
					state <= WRITE;
				end
				else begin
					state <= BUFFER5;
				end
			end
		end
					
		
	 WRITE: begin
		if(offset < 16)begin
			cur_write_data <= output_hash[offset+1];
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

bitcoin_sha256_single sha1 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_1),
	.message(w0),
	.starter_hash(h),
	.done(done_0),
	.bitcoin_simplified_sha256(ho)
	);

	bitcoin_sha256_single sha20 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w0),
	.starter_hash(ho),
	.done(done_20),
	.bitcoin_simplified_sha256(phase2_0hash)
	);
	
	bitcoin_sha256_single sha21 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w1),
	.starter_hash(ho),
	.done(done_1),
	.bitcoin_simplified_sha256(phase2_1hash)
	);
	
	bitcoin_sha256_single sha22 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w2),
	.starter_hash(ho),
	.done(done_2),
	.bitcoin_simplified_sha256(phase2_2hash)
	);
	
	bitcoin_sha256_single sha23 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w3),
	.starter_hash(ho),
	.done(done_3),
	.bitcoin_simplified_sha256(phase2_3hash)
	);
	
	bitcoin_sha256_single sha24 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w4),
	.starter_hash(ho),
	.done(done_4),
	.bitcoin_simplified_sha256(phase2_4hash)
	);
	
	bitcoin_sha256_single sha25 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w5),
	.starter_hash(ho),
	.done(done_5),
	.bitcoin_simplified_sha256(phase2_5hash)
	);
	
	bitcoin_sha256_single sha26 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w6),
	.starter_hash(ho),
	.done(done_6),
	.bitcoin_simplified_sha256(phase2_6hash)
	);
	
	bitcoin_sha256_single sha27 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_2),
	.message(w7),
	.starter_hash(ho),
	.done(done_7),
	.bitcoin_simplified_sha256(phase2_7hash)
	);
	
	bitcoin_sha256_single sha30 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w0),
	.starter_hash(h),
	.done(done_30),
	.bitcoin_simplified_sha256(phase3_0hash)
	);
	
	bitcoin_sha256_single sha31 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w1),
	.starter_hash(h),
	.done(done_31),
	.bitcoin_simplified_sha256(phase3_1hash)
	);
	
	bitcoin_sha256_single sha32 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w2),
	.starter_hash(h),
	.done(done_32),
	.bitcoin_simplified_sha256(phase3_2hash)
	);
	
	bitcoin_sha256_single sha33 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w3),
	.starter_hash(h),
	.done(done_33),
	.bitcoin_simplified_sha256(phase3_3hash)
	);
	
	bitcoin_sha256_single sha34 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w4),
	.starter_hash(h),
	.done(done_34),
	.bitcoin_simplified_sha256(phase3_4hash)
	);
	
	bitcoin_sha256_single sha35 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w5),
	.starter_hash(h),
	.done(done_35),
	.bitcoin_simplified_sha256(phase3_5hash)
	);
	
	bitcoin_sha256_single sha36 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w6),
	.starter_hash(h),
	.done(done_36),
	.bitcoin_simplified_sha256(phase3_6hash)
	);
	
	bitcoin_sha256_single sha37 (
	.clk(clk),
	.reset_n(reset_n),
	.start(start_3),
	.message(w7),
	.starter_hash(h),
	.done(done_37),
	.bitcoin_simplified_sha256(phase3_7hash)
	);
	
	
endmodule