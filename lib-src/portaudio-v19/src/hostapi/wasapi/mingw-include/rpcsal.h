#pragma once

#if __GNUC__ >=3
#pragma GCC system_header
#endif

#define RPC_range(min,max)

#define __RPC__in           
#define __RPC__in_string
#define __RPC__in_opt_string
#define __RPC__deref_opt_in_opt
#define __RPC__opt_in_opt_string
#define __RPC__in_ecount(size) 
#define __RPC__in_ecount_full(size)
#define __RPC__in_ecount_full_string(size)
#define __RPC__in_ecount_part(size, length)
#define __RPC__in_ecount_full_opt(size)
#define __RPC__in_ecount_full_opt_string(size)
#define __RPC__inout_ecount_full_opt_string(size)
#define __RPC__in_ecount_part_opt(size, length)

#define __RPC__deref_in 
#define __RPC__deref_in_string
#define __RPC__deref_opt_in
#define __RPC__deref_in_opt
#define __RPC__deref_in_ecount(size) 
#define __RPC__deref_in_ecount_part(size, length) 
#define __RPC__deref_in_ecount_full(size) 
#define __RPC__deref_in_ecount_full_opt(size)
#define __RPC__deref_in_ecount_full_string(size)
#define __RPC__deref_in_ecount_full_opt_string(size)
#define __RPC__deref_in_ecount_opt(size) 
#define __RPC__deref_in_ecount_opt_string(size)
#define __RPC__deref_in_ecount_part_opt(size, length) 

// [out]
#define __RPC__out     
#define __RPC__out_ecount(size) 
#define __RPC__out_ecount_part(size, length) 
#define __RPC__out_ecount_full(size)
#define __RPC__out_ecount_full_string(size)

// [in,out] 
#define __RPC__inout                                   
#define __RPC__inout_string
#define __RPC__opt_inout
#define __RPC__inout_ecount(size)                     
#define __RPC__inout_ecount_part(size, length)    
#define __RPC__inout_ecount_full(size)          
#define __RPC__inout_ecount_full_string(size)          

// [in,unique] 
#define __RPC__in_opt       
#define __RPC__in_ecount_opt(size)   


// [in,out,unique] 
#define __RPC__inout_opt    
#define __RPC__inout_ecount_opt(size)  
#define __RPC__inout_ecount_part_opt(size, length) 
#define __RPC__inout_ecount_full_opt(size)     
#define __RPC__inout_ecount_full_string(size)

// [out] **
#define __RPC__deref_out   
#define __RPC__deref_out_string
#define __RPC__deref_out_opt 
#define __RPC__deref_out_opt_string
#define __RPC__deref_out_ecount(size) 
#define __RPC__deref_out_ecount_part(size, length) 
#define __RPC__deref_out_ecount_full(size)  
#define __RPC__deref_out_ecount_full_string(size)


// [in,out] **, second pointer decoration. 
#define __RPC__deref_inout    
#define __RPC__deref_inout_string
#define __RPC__deref_inout_opt 
#define __RPC__deref_inout_opt_string
#define __RPC__deref_inout_ecount_full(size)
#define __RPC__deref_inout_ecount_full_string(size)
#define __RPC__deref_inout_ecount_opt(size) 
#define __RPC__deref_inout_ecount_part_opt(size, length) 
#define __RPC__deref_inout_ecount_full_opt(size) 
#define __RPC__deref_inout_ecount_full_opt_string(size) 

// #define __RPC_out_opt    out_opt is not allowed in rpc

// [in,out,unique] 
#define __RPC__deref_opt_inout  
#define __RPC__deref_opt_inout_string
#define __RPC__deref_opt_inout_ecount(size)     
#define __RPC__deref_opt_inout_ecount_part(size, length) 
#define __RPC__deref_opt_inout_ecount_full(size) 
#define __RPC__deref_opt_inout_ecount_full_string(size)

#define __RPC__deref_out_ecount_opt(size) 
#define __RPC__deref_out_ecount_part_opt(size, length) 
#define __RPC__deref_out_ecount_full_opt(size) 
#define __RPC__deref_out_ecount_full_opt_string(size)

#define __RPC__deref_opt_inout_opt      
#define __RPC__deref_opt_inout_opt_string
#define __RPC__deref_opt_inout_ecount_opt(size)   
#define __RPC__deref_opt_inout_ecount_part_opt(size, length) 
#define __RPC__deref_opt_inout_ecount_full_opt(size) 
#define __RPC__deref_opt_inout_ecount_full_opt_string(size) 

#define __RPC_full_pointer  
#define __RPC_unique_pointer
#define __RPC_ref_pointer
#define __RPC_string                               
