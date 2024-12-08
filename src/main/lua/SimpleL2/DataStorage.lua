local env = require "env"
local expect = env.expect

-- From SinkC
local dsWrite_s2 = ([[
    | valid
    | ready
    | set
    | wayOH
    | data
]]):bundle {hier = cfg.top, prefix = "io_dsWrite_s2_", name = "dsWrite_s2"}

local refillWrite_s2 = ([[
    | valid
    | set
    | wayOH
    | data
]]):bundle {hier = cfg.top, prefix = "io_refillWrite_s2_", name = "refillWrite_s2"}

local dsRead_s3 = ([[
    | valid
    | set
    | wayOH
    | dest
]]):bundle {hier = cfg.top, prefix = "io_fromMainPipe_dsRead_s3_", name = "dsRead_s3"}

local tempDS_write = ([[
    | valid
    | data
    | idx
]]):bundle {hier = cfg.top, prefix = "io_toTempDS_write_s6_", name = "tempDS_write_s6"}

local sourceD_data = ([[
    | valid
    | ready
    | data
]]):bundle {hier = cfg.top, prefix = "io_toSourceD_dsResp_s7s8_", name = "sourceD_data"}

local txdat_data = ([[
    | valid
    | ready
    | data
]]):bundle {hier = cfg.top, prefix = "io_toTXDAT_dsResp_s7s8_", name = "txdat_data"}

local TXDAT = ("0b0001"):number()
local SourceD = ("0b0010"):number()
local TempDataStorage = ("0b0100"):number()

local ds = dut.u_DataStorage

local function refill_data(set, wayOH, data_str)
    env.negedge()
        refillWrite_s2.valid:set(1)
        refillWrite_s2.bits.data:set_str(data_str)
        refillWrite_s2.bits.wayOH:set(wayOH)
        refillWrite_s2.bits.set:set(set)
    env.negedge()
        refillWrite_s2.valid:set(0)
end

local function read(set, wayOH, dest)
    env.negedge()
        dsRead_s3.valid:set(1)
        dsRead_s3.bits.set:set(set)
        dsRead_s3.bits.wayOH:set(wayOH)
        dsRead_s3.bits.dest:set(dest)
    env.negedge()
        dsRead_s3.valid:set(0)
end

local test_basic_read_write = env.register_test_case "test_basic_read_write" {
    function ()
        env.dut_reset()

        verilua "appendTasks" {
            check_task = function ()        
                local read_data = 0xff

                env.expect_happen_until(100, function (c)
                    return tempDS_write:fire()
                end)
                tempDS_write:dump()
                read_data = tempDS_write.bits.data:get()[1]
                expect.equal(read_data, 0xdead)
                tempDS_write.bits.idx:expect(4)

                env.posedge()
                env.expect_not_happen_until(100, function ()
                    return tempDS_write:fire()
                end)
            end
        }

        env.negedge()
            -- write "0xdead" into set=0x01, wayOH=0x01
            dsWrite_s2.ready:expect(1)
            dsWrite_s2.valid:set(1)
            dsWrite_s2.bits.set:set(2)
            dsWrite_s2.bits.data:set(0xdead, true)
        env.negedge()
            -- write way is provided in Stage 3
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(1)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(1)
            dsWrite_s2.valid:set(0)        
        env.negedge()
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(0)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(0)
        env.negedge()
            -- read from set=0x01, wayOH=0x01, mshrId_s3=0x04
            dsRead_s3.valid:set(1)
            dsRead_s3.bits.set:set(2)
            dsRead_s3.bits.wayOH:set(1)
            dsRead_s3.bits.dest:set(TempDataStorage)
            dut.io_fromMainPipe_mshrId_s3:set(4)

        env.negedge()
            dsRead_s3.valid:set(0)

        env.posedge(200)
    end
}

local test_refill_write = env.register_test_case "test_refill_write" {
    function ()
        env.dut_reset()

        verilua "appendTasks" {
            check_data_resp = function ()
                env.expect_happen_until(100, function()
                    return  tempDS_write.valid:get() == 1 and tempDS_write.bits.data:get()[1] == 0xdead
                end)
                tempDS_write:dump()
            end
        }

        env.negedge()
            refillWrite_s2.valid:set(1)
            refillWrite_s2.bits.data:set_str("0xdead")
            refillWrite_s2.bits.wayOH:set(("0b0010"):number())
            refillWrite_s2.bits.set:set(0x02)
        env.negedge()
            refillWrite_s2.valid:set(0)
        env.negedge(10)
            dsRead_s3.valid:set(1) -- read back
            dsRead_s3.bits.set:set(0x02)
            dsRead_s3.bits.wayOH:set(("0b0010"):number())
            dsRead_s3.bits.dest:set(TempDataStorage)
            dut.io_fromMainPipe_mshrId_s3:set(4)
        env.negedge()
            dsRead_s3.valid:set(0)
            
        env.posedge(100)
    end
}

local test_operate_diffrent_way = env.register_test_case "test_operate_diffrent_way" {
    function ()
        env.dut_reset()

        verilua "appendTasks" {
            function ()
                env.expect_happen_until(100, function () return tempDS_write:fire() end)
                tempDS_write:dump()
                tempDS_write.bits.idx:expect(4)
                -- tempDS_write.bits.data:expect_hex_str("0x0")
            end
        }

        env.negedge()
            refillWrite_s2.valid:set(1)
            refillWrite_s2.bits.data:set_str("0xdead")
            refillWrite_s2.bits.wayOH:set(("0b0010"):number())
            refillWrite_s2.bits.set:set(0x03)
        env.negedge()
            refillWrite_s2.valid:set(0)
            dsRead_s3.valid:set(1)
            dsRead_s3.bits.set:set(0x03)
            dsRead_s3.bits.wayOH:set(("0b0001"):number())
            dsRead_s3.bits.dest:set(TempDataStorage)
            dut.io_fromMainPipe_mshrId_s3:set(4)
        env.negedge()
            dsRead_s3.valid:set(0)

        env.posedge(100)
    end
}

local test_read_to_sourceD = env.register_test_case "test_read_to_sourceD" {
    function ()
        env.dut_reset()

        local function read_to_sourced(set, wayOH)
            env.negedge()
                dsRead_s3.valid:set(1)
                dsRead_s3.bits.set:set(set)
                dsRead_s3.bits.wayOH:set(wayOH)
                dsRead_s3.bits.dest:set(SourceD)
            env.negedge()
                dsRead_s3.valid:set(0)
        end

        refill_data(0x00, ("0b0001"):number(), "0xdead")
        refill_data(0x00, ("0b0010"):number(), "0xbeef")
        refill_data(0x00, ("0b0100"):number(), "0xabab")

        env.negedge(10)

        do
            sourceD_data.ready:set(1)

            -- normal read(no stall)
            read_to_sourced(0x00, ("0b0001"):number())
            read_to_sourced(0x00, ("0b0010"):number())

            env.expect_happen_until(10, function () return sourceD_data:fire() and sourceD_data.bits.data:is_hex_str("0xdead") end)
            env.expect_happen_until(10, function () return sourceD_data:fire() and sourceD_data.bits.data:is_hex_str("0xbeef") end)

            env.negedge(10)
        end

        do
            sourceD_data.ready:set(0)

            -- stall(two)
            read_to_sourced(0x00, ("0b0001"):number())
            read_to_sourced(0x00, ("0b0010"):number())

            env.negedge(10)

            env.negedge(10, function ()
                sourceD_data.valid:expect(1)
                sourceD_data.ready:expect(0)
                sourceD_data.bits.data:expect_hex_str("0xdead")
            end)

            env.negedge()
                sourceD_data.ready:set(1)
            env.posedge()
                sourceD_data.valid:expect(1)
                sourceD_data.ready:expect(1)
                sourceD_data.bits.data:expect_hex_str("0xdead")
            env.posedge()
                sourceD_data.valid:expect(1)
                sourceD_data.ready:expect(1)
                sourceD_data.bits.data:expect_hex_str("0xbeef")
            env.posedge()
            env.posedge(10, function () 
                sourceD_data.valid:expect(0) 
            end)
        end

        env.posedge(100)
    end
}

local test_read_stall = env.register_test_case "test_read_stall" {
    function ()
        env.dut_reset()
    
        sourceD_data.ready:set(0)

        refill_data(0x00, ("0b0001"):number(), "0xabcd")
        refill_data(0x00, ("0b0010"):number(), "0xbeef")

        env.negedge(10)

        -- one read request
        read(0x00, ("0b0001"):number(), SourceD)
        env.negedge(2)
            ds.ren_s6:expect(1)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(0)
        env.negedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(1)
            ds.ren_s8:expect(0)
            sourceD_data.valid:expect(1)
            expect.equal(sourceD_data.bits.data:get()[1], 0xabcd)
        env.negedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            expect.equal(sourceD_data.bits.data:get()[1], 0xabcd)
        env.negedge(math.random(1, 10))
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            expect.equal(sourceD_data.bits.data:get()[1], 0xabcd)
        env.negedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(1)
            sourceD_data.ready:set(1)
        env.posedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            expect.equal(sourceD_data.bits.data:get()[1], 0xabcd)
        env.negedge()
            ds.ren_s8:expect(0)
            sourceD_data.valid:expect(0)
        
        env.negedge(10)

        sourceD_data.ready:set(0)

        -- two read request
        read(0x00, ("0b0001"):number(), SourceD)
        read(0x00, ("0b0010"):number(), SourceD)
        env.negedge(2)
            ds.ren_s6:expect(1)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            sourceD_data.bits.data:expect_hex_str("0xabcd")
        env.negedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(1)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            sourceD_data.bits.data:expect_hex_str("0xabcd")
        env.negedge(math.random(1, 10))
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(1)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            sourceD_data.bits.data:expect_hex_str("0xabcd")
        env.negedge()
            sourceD_data.ready:set(1)
        env.posedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(1)
            ds.ren_s8:expect(1)
            sourceD_data.valid:expect(1)
            sourceD_data.bits.data:expect_hex_str("0xabcd")
        env.negedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(1)
            ds.ren_s8:expect(0)
            sourceD_data.valid:expect(1)
            sourceD_data.bits.data:expect_hex_str("0xbeef")
        env.negedge()
            ds.ren_s6:expect(0)
            ds.ren_s7:expect(0)
            ds.ren_s8:expect(0)
            sourceD_data.valid:expect(0)

        env.posedge(100)
    end
}

local test_basic_ecc_0bErr_read_write = env.register_test_case "test_basic_ecc_0bErr_read_write" {
    function ()
        env.dut_reset()

        verilua "appendTasks" {
            check_task = function ()        
                local read_data = 0xff

                env.expect_happen_until(100, function (c)
                    return tempDS_write:fire()
                end)
                tempDS_write:dump()
                read_data = tempDS_write.bits.data:get()[1]
                expect.equal(read_data, 0xdead)
                tempDS_write.bits.idx:expect(4)

                env.posedge()
                dut.u_DataStorage.rdDataHasErr_s7:expect(0)
                dut.u_DataStorage.io_eccError:expect(0)

                env.posedge()
                env.expect_not_happen_until(100, function ()
                    return tempDS_write:fire()
                end)
            end
        }

        env.negedge()
            -- write "0xdead" into set=0x01, wayOH=0x01
            dsWrite_s2.ready:expect(1)
            dsWrite_s2.valid:set(1)
            dsWrite_s2.bits.set:set(2)
            dsWrite_s2.bits.data:set(0xdead, true)
        env.negedge()
            -- write way is provided in Stage 3
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(1)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(1)
            dsWrite_s2.valid:set(0)        
        env.negedge()
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(0)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(0)
        env.negedge()
            -- read from set=0x01, wayOH=0x01, mshrId_s3=0x04
            dsRead_s3.valid:set(1)
            dsRead_s3.bits.set:set(2)
            dsRead_s3.bits.wayOH:set(1)
            dsRead_s3.bits.dest:set(TempDataStorage)
            dut.io_fromMainPipe_mshrId_s3:set(4)

        env.negedge()
            dsRead_s3.valid:set(0)

        env.posedge(200)
    end
}

local test_basic_ecc_1bErr_read_write = env.register_test_case "test_basic_ecc_1bErr_read_write" {
    function ()
        env.dut_reset()

        verilua "appendTasks" {
            check_task = function ()        
                local read_data = 0xff

                env.expect_happen_until(20, function () return sourceD_data:fire() end)
                sourceD_data:dump()
                sourceD_data.bits.data:expect_hex_str("0xdead")

                env.posedge()
                dut.u_DataStorage.rdDataHasErr_s7:expect(1)
                dut.u_DataStorage.rdDataHasUncorrectable_s7:expect(0)
                dut.u_DataStorage.io_eccError:expect(0)

                env.posedge()
                env.expect_not_happen_until(100, function () return sourceD_data:fire() end)
            end
        }

        env.negedge()
            -- write "0xdead" into set=0x01, wayOH=0x01
            dsWrite_s2.ready:expect(1)
            dsWrite_s2.valid:set(1)
            dsWrite_s2.bits.set:set(2)
            dsWrite_s2.bits.data:set(0xdead, true)
        env.negedge()
            -- write way is provided in Stage 3
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(1)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(1)
            dsWrite_s2.valid:set(0)        
        env.negedge()
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(0)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(0)
        env.negedge()
            -- read from set=0x01, wayOH=0x01, mshrId_s3=0x04
            dsRead_s3.valid:set(1)
            dsRead_s3.bits.set:set(2)
            dsRead_s3.bits.wayOH:set(1)
            dsRead_s3.bits.dest:set(SourceD)
            dut.io_fromMainPipe_mshrId_s3:set(4)
            -- dut.u_DataStorage.dataSRAMs_0_0.io_r_resp_data_0_1:set_force_str("0x01")
            dut.u_DataStorage.dataSRAMs_0_0.io_r_resp_data_0_0:set_force_str("0x7000000000001dead")
        env.negedge()
            dsRead_s3.valid:set(0)

        env.posedge(200)
            dut.u_DataStorage.dataSRAMs_0_0.io_r_resp_data_0_0:set_release()
    end
}

local test_basic_ecc_2bErr_read_write = env.register_test_case "test_basic_ecc_2bErr_read_write" {
    function ()
        env.dut_reset()

        verilua "appendTasks" {
            check_task = function ()        
                local read_data = 0xff

                env.expect_happen_until(100, function (c)
                    return sourceD_data:fire()
                end)
                sourceD_data:dump()
                read_data = sourceD_data.bits.data:get()[1]
                
                -- expect.equal(read_data, 0xdead)
                
                -- sourceD_data.bits.idx:expect(4)
                
                dut.u_DataStorage.io_eccError:expect(1) -- pulse
                env.posedge()
                dut.u_DataStorage.rdDataHasErr_s7:expect(1)
                dut.u_DataStorage.rdDataHasUncorrectable_s7:expect(1)


                env.posedge()
                env.expect_not_happen_until(100, function ()
                    return sourceD_data:fire()
                end)
            end
        }

        env.negedge()
            -- write "0xdead" into set=0x01, wayOH=0x01
            dsWrite_s2.ready:expect(1)
            dsWrite_s2.valid:set(1)
            dsWrite_s2.bits.set:set(2)
            dsWrite_s2.bits.data:set(0xdead, true)
        env.negedge()
            -- write way is provided in Stage 3
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(1)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(1)
            dsWrite_s2.valid:set(0)        
        env.negedge()
            dut.io_fromMainPipe_dsWrWayOH_s3_bits:set(0)
            dut.io_fromMainPipe_dsWrWayOH_s3_valid:set(0)
        env.negedge()
            -- read from set=0x01, wayOH=0x01, mshrId_s3=0x04
            dsRead_s3.valid:set(1)
            dsRead_s3.bits.set:set(2)
            dsRead_s3.bits.wayOH:set(1)
            dsRead_s3.bits.dest:set(SourceD)
            dut.io_fromMainPipe_mshrId_s3:set(4)
            dut.u_DataStorage.dataSRAMs_0_0.io_r_resp_data_0_1:set_force_str("0x11")
            -- dut:force_all()
            --     dut.u_DataStorage.dataSRAMs_0_0.io_r_resp_data_0_0:set_str("0x7000000000001dead")
        env.negedge()
            dsRead_s3.valid:set(0)

        env.posedge(200)
            dut.u_DataStorage.dataSRAMs_0_0.io_r_resp_data_0_1:set_release()
    end
}

verilua "mainTask" {
    function ()
        sim.dump_wave()

        test_basic_read_write()
        test_refill_write()
        test_operate_diffrent_way()
        test_read_to_sourceD()
        test_read_stall()

        test_basic_ecc_0bErr_read_write()
        
        -- TODO:
        -- test_basic_ecc_1bErr_read_write()
        -- test_basic_ecc_2bErr_read_write()
        
        env.posedge(100)
        env.TEST_SUCCESS()
    end
}
