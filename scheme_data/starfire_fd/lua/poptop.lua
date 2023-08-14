--[[
   @see xkjd6 https://gitee.com/xkinput/Rime_JD/blob/master/rime/lua/for_topup.lua
   ------------
   Schema配置
   ------------
   1. 将poptop.lua添加至rime.lua
   poptop_processor = require("poptop")
   
   2. 将poptop_processor挂接在speller之前
   processors:
   ...
   - lua_processor@poptop_processor
   - speller
   ...
   
   3. 配置器直接修改本脚本init部分，意义如下：
   poptop_with: "aeuio" # 顶功码
   min_length: 2  # 最小长度
   max_length: 4  # 最大全码长
   auto_clear: true  # 顶功空码时是否清空输入
   poptop_command: true # 首码为顶功码时是否禁用顶功逻辑（如orq）
]]

local function string2set(str)
   local t = {}
   for i = 1, #str do
      local c = str:sub(i,i)
      t[c] = true
   end
   return t
end

local function poptop(env)
   if not env.engine.context:get_selected_candidate() then
      if env.auto_clear then
         env.engine.context:clear()
      end
   else
      env.engine.context:commit()
   end
end

local function selector(ch, context)
   if ch == 0x61 then
      context:select(1)
   elseif ch == 0x65 then
      context:select(2)
   elseif ch == 0x75 then
      context:select(3)
   elseif ch == 0x69 then
      context:select(4)
   elseif ch == 0x6F then
      context:select(5)
   else
      return 2
   end

   context:commit()
   return 1
end

local function processor(key_event, env)
   local engine = env.engine
   local schema = engine.schema
   local context = engine.context

   local input = context.input 
   local min_len = env.poptop_min

   if key_event:release() or key_event:ctrl() or key_event:alt() then
      return 2
   end

   local ch = key_event.keycode

   if ch < 0x20 or ch >= 0x7f then
      return 2
   end

   local key = string.char(ch)
   local prev = string.sub(input, -1)
   local first = string.sub(input, 1, 1)
   if #first == 0 then
      first = key
   end

   local is_alphabet = env.alphabet[key] or false
   local is_poptop = env.poptop_set[key] or false
   local is_prev_poptop = env.poptop_set[prev] or false
   local is_first_poptop = env.poptop_set[first] or false


   log.info('first_poptop judge ')
   if env.poptop_command and is_first_poptop then
      return 2
   end

   log.info('alphabet judge ')
   if not is_alphabet then
      log.info('alphabet judge ')
      return 2
   end

   if is_prev_poptop and not is_poptop then
      log.info('prev poptop current not')
      poptop(env)
   elseif not is_prev_poptop and not is_poptop and #input >= min_len then
      log.info('prev not poptop, current not')
      poptop(env)
   elseif #input >= env.poptop_max then
      return selector(ch, context)
   end

   return 2
end

local function init(env)
   local config = env.engine.schema.config

   env.poptop_set = string2set("aeuio")
   env.alphabet = string2set(config:get_string("speller/alphabet"))
   env.poptop_min = 2
   env.poptop_max = 4
   env.auto_clear = true
   env.poptop_command = true
   env.enabled = true
end

return { init = init, func = processor }
