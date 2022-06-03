module simplified_sha256(
 input logic  clk, reset_n, start,
 input logic [31:0] message[16], 
 input logic [31:0] in[8],
 input logic [31:0] k[0:63],
 output logic done,
 output logic [31:0] sha256[8]);

enum logic [2:0] {IDLE, BLOCK, PIPE_A, PIPE_B, COMPUTE, WRITE, DONE} state;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, hash, P_sum;
logic [ 6:0] num;
logic [31:0] w[16];

function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, P_sum);
    logic [31:0] S1, S0, ch, maj, t1, t2; 
	begin
		S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
		ch = (e & f) ^ ((~e) & g);
		t1 = S1 + ch + P_sum;
		S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
		maj = (a & b) ^ (b & c) ^ (c & a);
		t2 = S0 + maj;
		sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
	end
endfunction

function logic [31:0] wtnew;
	logic [31:0] S1, S0;
	begin
		S0 = rightrotate(w[1], 7) ^ rightrotate(w[1], 18) ^ (w[1] >> 3);
		S1 = rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14] >> 10);
		wtnew = w[0] + S0 + w[9] + S1;
	end
endfunction	

function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
begin
	rightrotate = (x >> r) | (x << (32-r));
end
endfunction

always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    state <= IDLE;
  end 
  else case (state)
    IDLE: begin 
       if(start) begin
			h0 <= in[0];
			h1 <= in[1];
			h2 <= in[2];
			h3 <= in[3];
			h4 <= in[4];
			h5 <= in[5];
			h6 <= in[6];
			h7 <= in[7]; 	
			
			a <= in[0];
			b <= in[1];
			c <= in[2];
			d <= in[3];
			e <= in[4];
			f <= in[5];
			g <= in[6];
			hash <= in[7]; 
			
			num <= 7'b0;
			state <= BLOCK;
       end
    end
		
    BLOCK: begin
		for(int n=0; n<16; n++) w[n] <= message[n];
		state <= PIPE_A;
    end

	 PIPE_A: begin
			P_sum <= w[0] + k[0] + hash;
			for(int n=0; n<15; n++) w[n] <= w[n + 1];
			w[15] <= wtnew;
			state <= PIPE_B;
	 end

	 PIPE_B: begin
		{a, b, c, d, e, f, g, hash} <= sha256_op(a, b, c, d, e, f, g, P_sum);
		P_sum <= w[0] + k[1] + g;
		for(int n=0; n<15; n++) w[n] <= w[n + 1];
		w[15] <= wtnew;
		num <= num+1;
		state <= COMPUTE;
	 end
	 
	 
    COMPUTE: begin
		if(num < 64) begin
			P_sum <= w[0] + k[num+1] + g;
			{a, b, c, d, e, f, g, hash} <= sha256_op(a,b,c,d,e,f,g,P_sum);
			for(int j=0;j<15;j++) w[j] <= w[j+1];
			w[15] <= wtnew;
			num <= num + 1;
			state <= COMPUTE;
		end
	   else begin
		 h0 <= a + h0;
		 h1 <= b + h1;
		 h2 <= c + h2;
		 h3 <= d + h3;
		 h4 <= e + h4;
		 h5 <= f + h5;
		 h6 <= g + h6;
		 h7 <= hash + h7;
		 num <= 0;
		 state <= WRITE;
     end
	 end
	 
    WRITE: begin
		sha256[0] <= h0;
		sha256[1] <= h1;
		sha256[2] <= h2;
		sha256[3] <= h3;
		sha256[4] <= h4;
		sha256[5] <= h5;
		sha256[6] <= h6;
		sha256[7] <= h7;
		state <= DONE;
		end
    
	 DONE: begin
		state <= IDLE;
	end
   endcase
  end

assign done = (state == DONE);

endmodule
