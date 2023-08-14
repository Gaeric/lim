function aeuio_key_binder(key_event, env)
   log.info("aeuio key binder load")

   local ch = key_event.keycode
   log.info("key_event key is " .. ch)

   local context = env.engine.context
   log.info("the input string is " .. context.input)

   if #context.input == 2 then
      if ch == 0x61 then
         context:select(1)
         log.info("a match ")
      elseif ch == 0x65 then
         log.info("e match ")
         context:select(2)
         -- e
      elseif ch == 0x75 then
         log.info("u match ")
         context:select(3)
         -- u
      elseif ch == 0x69 then
         log.info("i match ")
         context:select(4)
         -- i
      elseif ch == 0x6F then
         log.info("o match ")
         context:select(5)
      end
      return 1

   end

   return 2



   -- local schema = env.engine.schema
   -- log.info("schema id is " ..  schema.schema_id)
   -- local composition = env.engine.context.composition
   -- if composition:has_finished_composition() then
   --    log.info("finished composition")
   -- else
   --    log.info("not finished composition")
   -- end

   -- context:commit()
end


function demo_translator()
   log.info("demo translator load")
end
