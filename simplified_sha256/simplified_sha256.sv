module simplified_sha256 #(parameter integer NUM_OF_WORDS = 20)(
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);

// FSM state variables 
enum logic [3:0] {IDLE, REST, READ, BLOCK1, BLOCK2, PIPE_A, PIPE_B, PIPE_C, PIPE_D, COMPUTE_1, COMPUTE_2, BUFFER, WRITE} state;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
logic [31:0] message[4];
logic [31:0] h[8];
logic [31:0] a, b, c, d, e, f, g, hash, P;
logic [ 6:0] num;
logic [ 4:0] offset; // in word address
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;

// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, P);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
	begin
		S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
		ch = (e & f) ^ ((~e) & g);
		t1 = S1 + ch + P;
		S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
		maj = (a & b) ^ (a & c) ^ (b & c);
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
// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;

function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
begin
	rightrotate = (x >> r) | (x << (32-r));
end
endfunction


// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
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
			state <= REST;
       end
    end

	 REST:begin
		state <= READ;
    end 
	 
	  READ: begin
			if(offset < NUM_OF_WORDS) begin
				if(offset < 16) begin
					w[offset] <= mem_read_data;
					offset <= offset + 1;
					state <= REST;
				end
				else begin
					message[offset - 16] <= mem_read_data;
					offset <= offset + 1;
					state <= REST;
			  end 
			end
			else begin		
				offset <= 0;
				state <=BLOCK1;	
			end
		end			
		
		BLOCK1: begin
			a <= h[0];
			b <= h[1];
			c <= h[2];
			d <= h[3];
			e <= h[4];
			f <= h[5];
			g <= h[6];
			hash <= h[7];
			state <= PIPE_A;
		end
	
	 PIPE_A: begin
			P <= w[0] + k[0] + hash;
			for(int n=0; n<15; n++) w[n] <= w[n + 1];
			w[15] <= wtnew;
			state <= PIPE_B;
	 end
	 
	 PIPE_B: begin
		{a, b, c, d, e, f, g, hash} <= sha256_op(a,b,c,d,e,f,g, P);
		P <= w[0] + k[1] + g;
		for(int n=0; n<15; n++) w[n] <= w[n + 1];
		w[15] <= wtnew;
		num <= num+1;
		state <= COMPUTE_1;
	 end
	 
	 COMPUTE_1: begin
		if(num < 64) begin
			P <= w[0] + k[num+1] + g;
			{a, b, c, d, e, f, g, hash} <= sha256_op(a,b,c,d,e,f,g,P);
			for(int j=0;j<15;j++) w[j] <= w[j+1];
			w[15] <= wtnew;
			num <= num + 1;
			state <= COMPUTE_1;
		end
	   else begin
		 h[0] <= a + h[0];
		 h[1] <= b + h[1];
		 h[2] <= c + h[2];
		 h[3] <= d + h[3];
		 h[4] <= e + h[4];
		 h[5] <= f + h[5];
		 h[6] <= g + h[6];
		 h[7] <= hash + h[7];
		 num <= 0;
		 state <= BLOCK2;
     end
	 end
	 
	 BLOCK2: begin
			a <= h[0];
			b <= h[1];
			c <= h[2];
			d <= h[3];
			e <= h[4];
			f <= h[5];
			g <= h[6];
			hash <= h[7];
			
			for(int n=0; n<4; n++) w[n] <= message[n];
			w[4] <= 32'h80000000;
			w[15] <= 32'd640;
			for(int j=5; j<15; j++) w[j] <= 32'h0;
			state <= PIPE_C;
		end
	 
	 PIPE_C: begin
			P <= w[0] + k[0] + hash;
			for(int n=0; n<15; n++) w[n] <= w[n + 1];
			w[15] <= wtnew;
			state <= PIPE_D;
	 end
	 
	 PIPE_D: begin
		{a, b, c, d, e, f, g, hash} <= sha256_op(a,b,c,d,e,f,g, P);
		P <= w[0] + k[1] + g;
		for(int n=0; n<15; n++) w[n] <= w[n + 1];
		w[15] <= wtnew;
		num <= num+1;
		state <= COMPUTE_2;
	 end
	 
	 COMPUTE_2: begin
		if(num < 64) begin
			P <= w[0] + k[num+1] + g;
			{a, b, c, d, e, f, g, hash} <= sha256_op(a,b,c,d,e,f,g,P);
			for(int j=0;j<15;j++) w[j] <= w[j+1];
			w[15] <= wtnew;
			num <= num + 1;
			state <= COMPUTE_2;
		end
	   else begin
		 h[0] <= a + h[0];
		 h[1] <= b + h[1];
		 h[2] <= c + h[2];
		 h[3] <= d + h[3];
		 h[4] <= e + h[4];
		 h[5] <= f + h[5];
		 h[6] <= g + h[6];
		 h[7] <= hash + h[7];
		 num <= 0;
		 state <= BUFFER;
     end
	 end

	 BUFFER: begin
		cur_we <= 1;
		cur_addr <= output_addr;
		cur_write_data <= h[0];
		state <= WRITE;
	 end
    WRITE: begin
		if(offset < 8)begin
			cur_write_data <= h[offset+1];
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

endmodule
