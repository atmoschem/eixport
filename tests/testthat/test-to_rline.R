context("to_rline")

data(emisco)
emisco <- st_explode(emisco)
emisco$V8 <- units::set_units(emisco$V8, "g/ms")
Source <- to_rline(Emis = emisco["V8"],
                    Z_b =0,
                    Z_e =0,
                    dCL = 0,
                    sigmaz0 = 2,
                    lanes = 1)

Source <- to_rline(Emis = emisco["V8"],
                   Z_b =0,
                   Z_e =0,
                   dCL = 0,
                   sigmaz0 = 2,
                   lanes = 1,
                   experimental = TRUE,
                   Hw1 = 0,
                   dw1 = 0,
                   Hw2 = 0,
                   dw2 = 0,
                   Depth = 0,
                   Wtop = 0,
                   Wbottom = 0)

