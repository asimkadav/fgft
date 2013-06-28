// Provide a pointer disp_kern to misc device
register_miscfn (disp_kern, function_id_map, function_id_map_len);

// Register all the other functions/globals with the OT.
register_functions ();
register_globals ();
