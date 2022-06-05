
module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;
int  NUM_OF_BLOCK_WORDS = 16;
//logic[15:0] sha1_mem_out_addr;
logic [ 4:0] state;
integer nonce_count = 0;
logic[31:0] to_pass = 32'h0;
logic phase1_done;
logic phase2_done;
logic phase3_done;
logic [15:0] cur_addr;
logic [31:0] w_0[16];
logic [31:0] w_1[16];
logic [31:0] w_2[16];
logic [7:0] i;
//logic[4:0] offset // the offset has to be 5 bits long because the offset can be as large as 19 to read the first 19 words from memory.
//assign mem_addr
logic complete_phase1;
logic [31:0] hout[num_nonces];
logic [15:0] second_block_start;
logic [31:0] message[16];// to hold all the possible 32 bit mwords in 16 word input to a sha
logic[31:0] phase1_hash[8];
logic [15:0] offset;
logic[31:0] phase2_input_hash[8];
logic[31:0] phase3_input_hash[8];
logic[31:0] phase3_hash[8];
logic[31:0] phase2_hash[8];
logic start_phase1;
logic start_phase2;
logic start_phase3;
assign mem_addr = cur_addr + offset;
assign mem_clk = clk;

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};
logic [31:0] starter_hash[8] ;
initial begin
 starter_hash[0] = 32'h6a09e667;
 starter_hash[1] = 32'hbb67ae85;
 starter_hash[2] = 32'h3c6ef372;
 starter_hash[3] = 32'ha54ff53a;
 starter_hash[4] = 32'h510e527f;
 starter_hash[5] = 32'h9b05688c;
 starter_hash[6]= 32'h1f83d9ab;
 starter_hash[7] = 32'h5be0cd19;
    
end
 
// Student to add rest of the code here

// perform hash on first block of data


// send first 6 wor


always_ff @(posedge clk or negedge reset_n) begin : proc_
    if(~reset_n) begin
         state <= 0;
         complete_phase1 <= 0;
			mem_we <= 1'b0;
         NUM_OF_BLOCK_WORDS <= 16;

    end else begin
         case (state)

        /*Idle state*/ 0:  begin
            $display("we are in state 0");
									if(start == 1)
                            begin
                                //load the data into pase 1 sha has to be word w[0] to w[15]
                                cur_addr <= message_addr;
                                offset <= 16'b0;
                                state <= 1;
										  mem_we <= 1'b0;
                                complete_phase1 <= 0;
                              /*  sha1_mem_addr <= message_addr
                                sha1_mem_out_addr <= output_addr
                                start_phase1<= start;
                                phase1_we <= mem_we;*/

                                end
										  else
										  begin
										  state <= 0;
										  end

                            end
									 
                   /*read in the words from memory to comprise a full block */
                     1:  begin//1
                                    $display("we are in state 1");

                             if(offset < 16)
                             begin//2
                             if(offset < NUM_OF_BLOCK_WORDS)
                             begin//3
                            message[offset] <= mem_read_data;
                             offset <= offset +1;
                            state <= 1;
                              end//-3
                             else 
                             begin//4
                             if (offset == NUM_OF_BLOCK_WORDS)
                              begin//5
                            message[offset] <= 32'h80000000;
                              end//-5
                             else if (offset == 15 &&(NUM_OF_BLOCK_WORDS < 16)) 
                             begin//6
                                    message[offset] <=  32'd640;
                                end//-6
									 else 
                             begin//7
                              message[offset] <= 32'h0;
                             end//-7                             
                            offset <= offset + 1;
                            state <= 1;
                            end//-4
                            end//-2
                            else 
                            begin//8
                          
                            if(complete_phase1 == 1'b1)
                            begin//9
                              offset <= 0;
                            i<= 0;
                            state <= 5;

                             end//-9
                             else
                            begin//10
                            second_block_start<= mem_addr;
                              offset <= 0;
                            i<= 0;
                            state <= 2;
                            end//-10
                            
                             end//-8
                            /*sha1_message_addr <= message_addr
                               // sha1_mem_out_addr <= output_addr
                                start_phase1<= 1'b1;
                                //phase1_we <= mem_we;
                                state <= 2*/
                            end
                        /* SAVE THE WORDS read from the first block into the vector W_0 and then transtion to activating the sha phase*/
                        2:  begin
                                                                $display("we are in state 2");

                            for(int j = 0; j < 16;j++)
                            begin
                                w_0[j] <= message[j];
                            end
                            state <= 3;
									 end
								/*Activate sha phase 1*/
                        3: begin  
                                                                $display("we are in state 3");

                            start_phase1 <= 1'b1;
                            state <= 4;
                        end
								/* When sha phase 1 has conluded,set the memory address pointer to the start of the second block and also turn off phase 1 sha*/
                        4: begin
                                                                $display("we are in state 4");

                            wait(phase1_done == 1'b1);
                            start_phase1 <= 1'b0;
                            complete_phase1 <= 1'b1;
                            cur_addr <=second_block_start;
                            NUM_OF_BLOCK_WORDS <= 4;
                            state<= 1;
                            end 
									 
									/*save the words from the second block into a vector for phase2, also set the nonce bit to a value it hasnt been set to before*/

                        5: begin
                                                                $display("we are in state 5");

                            for(int k = 0; k < 16;k++)
                            begin
                                w_1[k] <= message[k];
                            end
                            w_1[3] <= to_pass;


									 //pass the hash output from phase1 into phase 2
                            phase2_input_hash <= phase1_hash;

                            state <= 6;
									 end
									 
									/*start phase 2*/
                        6: begin
                                                                $display("we are in state 6");
                                                                                            $display("w_1[3] = %d", w_1[3]);


                            start_phase2 <= 1'b1;
                            state <= 7;
                        end
									// when phase 2 has cocluded, swithc it off and transition to building the input for phase 2
                        7: begin
                                                                $display("we are in state 7");

                             wait(phase2_done == 1'b1);
                            start_phase2 <= 1'b0;
                            //complete_phase1 <= 1'b1;
                           // cur_addr <=second_block_start;
                            state<= 8;
                            end 
									 
									 //building the input for phase 3 involves opying the first 8 words from output of phase 1 and then padding the rest
                        8: begin
                                                                                            $display("we are in state 8");

                            if( i < 16)
                            begin
                                if(i < 8)
                                    begin
                                        w_2[i] <= phase2_hash[i];
                                        i<= i +1;
                                        state <= 8;
                                    end
                                else
                                begin
                                    if (i == 8) begin
                                        w_2[i] <= 32'h80000000;
                                    end
                                    else if (i == 15) begin
                                        w_2[i] <= 32'd256;
                                    end
                                    else
                                    begin
                                        w_2[i] <= 32'h0;
                                    end

                                    i <= i + 1;
                                    state <= 8;
                                end
                            end
                            else
                                begin
                                    i <= 0;
                                    state <= 9;
                                end
                            end
									//start phase 3
                        9: begin
                                                                $display("we are in state 9");

                            start_phase3 <= 1'b1;
                            state <= 10;
                                end
								//terminate phase 3 when it is condluded, and also take the 0th value of its output and add it to the final output, also increment the nonce count
                         10: begin
                                                                                            $display("we are in state 10");

                             wait(phase3_done == 1'b1);
                            start_phase3 <= 1'b0;
                            hout[nonce_count] <= phase3_hash[0];
                             $display("phase3_hash[0] = %d", phase3_hash[0]);
                            nonce_count <= nonce_count + 1;
                            to_pass<= to_pass +1;
                            $display("nonce_count = %d", nonce_count);

                            state<= 11;
                            end 
								//check if all nonces have been tried, if so switch to write state, if not redo phase2 
                        11: begin

                            if (nonce_count == 16) begin
                                cur_addr <= output_addr;
										  mem_we <= 1'b1;
                                state<= 12;
                            end
                            else
                            begin
                                cur_addr <=second_block_start;
                                NUM_OF_BLOCK_WORDS <= 4;
                                complete_phase1 <= 1'b1;
                                //forphase3 <= 1'b1;
                                state <= 1;
                            end
                          $display("we are in state 11 with nonce_count = %d", nonce_count);

									 end
							//start writing the output into memory anad mark done flag when completed

                        12: begin
                                                                                            $display("we are in state 12");

                            if(offset < 16)
                                begin
                                    mem_write_data <= hout[offset];
                                    offset <= offset + 1;
                                    state <= 12;
                                end
                                else
                                    begin
                                        offset <= 0;
                                        done <= 1'b1;
                                        state <= 0;
                                    end
                            end


                            //



         
             default : begin
				 state <= 0;
				 end
         endcase
    end
end
//assign done = (state == 0);

        simplified_sha256 phase1(
         .start(start_phase1),
        .message(w_0),
        .clk(clk),
        .done(phase1_done),
        .reset_n(reset_n),
        .k(k),
        .in(starter_hash),
        .sha256(phase1_hash)
         );

        simplified_sha256 phase2(
        .reset_n(reset_n),
         .clk(clk),
          .start(start_phase2), .in(phase2_input_hash), .done(phase2_done), .k(k), .message(w_1), .sha256(phase2_hash) );

        simplified_sha256  phase3(
            .reset_n(reset_n),
            .clk(clk), .in     (starter_hash), .sha256 (phase3_hash), .k      (k), .message(w_2), .done   (phase3_done), .start  (start_phase3));


    /*bitcoin_simplified_sha256 #(.NUM_OF_TOTAL_WORDS(20), .NUM_OF_BLOCK_WORDS(16)) phase1(
        .start           (start_phase1),
         .clk             (clk),
         .reset_n         (reset_n),
        .done            (phase1_done),
         .out_hash        (phase1_hash),
         .start_hash_const(starter_hash),
         .input_words     (w_0)
         );*/

    /*bitcoin_simplified_sha256 #(.NUM_OF_TOTAL_WORDS(20), .NUM_OF_BLOCK_WORDS(4)) phase2(
       .start           (start_phase2),
        .reset_n         (reset_n),
         .clk             (clk),
        .input_words     (w_1),
        .done            (phase2_done),
        .start_hash_const(phase2_input_hash),
         .out_hash        (phase2_hash) );

    bitcoin_simplified_sha256 #(.NUM_OF_TOTAL_WORDS(20), .NUM_OF_BLOCK_WORDS(4)) phase3(
        .start           (start_phase3),
        .reset_n         (reset_n),
        .clk             (clk),
        .out_hash        (phase3_hash),
        .done            (phase3_done), 
        .start_hash_const(starter_hash),
        .input_words     (w_2) );*/

    
endmodule
