--Vector 3
local type=type;local setmetatable=setmetatable;local tostring=tostring;local a=math.pi;local b=math.min;local c=math.max;local d=math.deg;local e=math.rad;local f=math.sqrt;local g=math.sin;local h=math.cos;local i=math.atan;local j=math.acos;local k=math.fmod;local l={}l.__index=l;function Vector3(m,n,o)if type(m)~="number"then m=0.0 end;if type(n)~="number"then n=0.0 end;if type(o)~="number"then o=0.0 end;m=m or 0.0;n=n or 0.0;o=o or 0.0;return setmetatable({x=m,y=n,z=o},l)end;function l.__eq(p,q)return p.x==q.x and p.y==q.y and p.z==q.z end;function l.__unm(p)return Vector3(-p.x,-p.y,-p.z)end;function l.__add(p,q)local r=type(p)local s=type(q)if r=="table"and s=="table"then return Vector3(p.x+q.x,p.y+q.y,p.z+q.z)elseif r=="table"and s=="number"then return Vector3(p.x+q,p.y+q,p.z+q)elseif r=="number"and s=="table"then return Vector3(p+q.x,p+q.y,p+q.z)end end;function l.__sub(p,q)local r=type(p)local s=type(q)if r=="table"and s=="table"then return Vector3(p.x-q.x,p.y-q.y,p.z-q.z)elseif r=="table"and s=="number"then return Vector3(p.x-q,p.y-q,p.z-q)elseif r=="number"and s=="table"then return Vector3(p-q.x,p-q.y,p-q.z)end end;function l.__mul(p,q)local r=type(p)local s=type(q)if r=="table"and s=="table"then return Vector3(p.x*q.x,p.y*q.y,p.z*q.z)elseif r=="table"and s=="number"then return Vector3(p.x*q,p.y*q,p.z*q)elseif r=="number"and s=="table"then return Vector3(p*q.x,p*q.y,p*q.z)end end;function l.__div(p,q)local r=type(p)local s=type(q)if r=="table"and s=="table"then return Vector3(p.x/q.x,p.y/q.y,p.z/q.z)elseif r=="table"and s=="number"then return Vector3(p.x/q,p.y/q,p.z/q)elseif r=="number"and s=="table"then return Vector3(p/q.x,p/q.y,p/q.z)end end;function l.__tostring(p)return"( "..p.x..", "..p.y..", "..p.z.." )"end;function l:clear()self.x=0.0;self.y=0.0;self.z=0.0 end;function l:unpack()return self.x,self.y,self.z end;function l:length_2d_sqr()return self.x*self.x+self.y*self.y end;function l:length_sqr()return self.x*self.x+self.y*self.y+self.z*self.z end;function l:length_2d()return f(self:length_2d_sqr())end;function l:length()return f(self:length_sqr())end;function l:dot(t)return self.x*t.x+self.y*t.y+self.z*t.z end;function l:cross(t)return Vector3(self.y*t.z-self.z*t.y,self.z*t.x-self.x*t.z,self.x*t.y-self.y*t.x)end;function l:dist_to(t)return(t-self):length()end;function l:is_zero(u)u=u or 0.001;if self.x<u and self.x>-u and self.y<u and self.y>-u and self.z<u and self.z>-u then return true end;return false end;function l:normalize()local v=self:length()if v<=0.0 then return 0.0 end;self.x=self.x/v;self.y=self.y/v;self.z=self.z/v;return v end;function l:normalize_no_len()local v=self:length()if v<=0.0 then return end;self.x=self.x/v;self.y=self.y/v;self.z=self.z/v end;function l:normalized()local v=self:length()if v<=0.0 then return Vector3()end;return Vector3(self.x/v,self.y/v,self.z/v)end;function clamp(w,x,y)if w<x then return x elseif w>y then return y end;return w end;function normalize_angle(z)local A;local B;B=tostring(z)if B=="nan"or B=="inf"then return 0.0 end;if z>=-180.0 and z<=180.0 then return z end;A=k(k(z+360.0,360.0),360.0)if A>180.0 then A=A-360.0 end;return A end;function vector_to_angle(C)local v;local D;local E;v=C:length()if v>0.0 then D=d(i(-C.z,v))E=d(i(C.y,C.x))else if C.x>0.0 then D=270.0 else D=90.0 end;E=0.0 end;return Vector3(D,E,0.0)end;function angle_forward(z)local F=g(e(z.x))local G=h(e(z.x))local H=g(e(z.y))local I=h(e(z.y))return Vector3(G*I,G*H,-F)end;function angle_right(z)local F=g(e(z.x))local G=h(e(z.x))local H=g(e(z.y))local I=h(e(z.y))local J=g(e(z.z))local K=h(e(z.z))return Vector3(-1.0*J*F*I+-1.0*K*-H,-1.0*J*F*H+-1.0*K*I,-1.0*J*G)end;function angle_up(z)local F=g(e(z.x))local G=h(e(z.x))local H=g(e(z.y))local I=h(e(z.y))local J=g(e(z.z))local K=h(e(z.z))return Vector3(K*F*I+-J*-H,K*F*H+-J*I,K*G)end;function get_FOV(L,M,N)local O;local P;local Q;local R;P=angle_forward(L)Q=(N-M):normalized()R=j(P:dot(Q)/Q:length())return c(0.0,d(R))end

--Includes
local function includes( table, key ) local state = false for i=1, #table do if table[i] == key then state = true break end end return state end

--Anti aim
local Pitch = ui.reference( "AA", "Anti-aimbot angles", "Pitch" )
local YawBase = ui.reference( "AA", "Anti-aimbot angles", "Yaw base" )
local Yaw = { ui.reference( "AA", "Anti-aimbot angles", "Yaw" ) }
local Jitter = { ui.reference( "AA", "Anti-aimbot angles", "Yaw jitter") }
local Body = { ui.reference( "AA", "Anti-aimbot angles", "Body yaw" ) }
local FreestandingBodyYaw = ui.reference( "AA", "Anti-aimbot angles", "Freestanding body yaw" )
local LowerBodyYawTarget = ui.reference( "AA", "Anti-aimbot angles", "Lower body yaw target" )
local FakeYawLimit = ui.reference( "AA", "Anti-aimbot angles", "Fake yaw limit" )
local EdgeYaw = ui.reference( "AA", "Anti-aimbot angles", "Edge yaw")
local Freestanding_ref = { ui.reference( "AA", "Anti-aimbot angles", "Freestanding" ) }
local onshot = { ui.reference( "AA", "Other", "On shot anti-aim") }
local slowwalk = { ui.reference( "AA", "Other", "Slow motion") }
local slowwalk_type = ui.reference( "AA", "Other", "Slow motion type")
local fakepeek = { ui.reference( "AA", "Other", "Fake peek") }
local leg = ui.reference( "AA", "Other", "Leg movement" )

--Fake lag
local fakelag = { ui.reference( "AA", "Fake lag", "Enabled" ) }
local amount = ui.reference( "AA", "Fake lag", "Amount" )
local variance = ui.reference( "AA", "Fake lag", "Variance" )
local limit = ui.reference( "AA", "Fake lag", "Limit")

--Aimbot
local PreferSafe = ui.reference( "Rage", "Aimbot", "Prefer safe point")
local Forcesafe = ui.reference( "Rage", "Aimbot", "Force safe point")
local hitchance = ui.reference( "Rage", "Aimbot", "Minimum hit chance")
local mindmg = ui.reference( "Rage", "Aimbot", "Minimum damage")
local delayshots = ui.reference( "Rage", "Other", "Delay shot")
local preferbaim = ui.reference( "Rage", "Other", "Prefer body aim")
local disablers = ui.reference( "Rage", "Other", "Prefer body aim disablers")
local forcebodyaim = ui.reference( "Rage", "Other", "Force body aim")
local fakeduck = ui.reference( "Rage", "Other", "Duck peek assist" )
local doubletap = { ui.reference( "Rage", "Other", "Double tap" ) }
local dtmode = ui.reference( "Rage", "Other", "Double tap mode" )
local maxusr = ui.reference("MISC", "Settings", "sv_maxusrcmdprocessticks")

--Visuals
local bulletimpacts = { ui.reference("Visuals", "Effects", "Bullet impacts" ) }
local thirdperson = { ui.reference( "Visuals", "Effects", "Force third person (alive)") }

local build = 1
local version = "Live"

local Blackboy = ui.new_multiselect( "MISC", "Settings", "AllStars", { "Anti-Aim", "Aimbot", "Visuals" } )

local aa = {
    ["base"] = ui.new_combobox( "AA", "Anti-aimbot angles", "Base", { "Freestanding", "Reversed freestanding", "Switch" } ),
    ["lean_left"] = ui.new_slider( "AA", "Anti-aimbot angles", "Lean  \n left", -25, 100, 50, true, "%", 1 ),
    ["lean_right"] = ui.new_slider( "AA", "Anti-aimbot angles", "\n Lean right", -25, 100, 50, true, "%", 1 ),
    ["delta"] = ui.new_slider( "AA", "Anti-aimbot angles", "Delta", 0, 60, 45, true, "°", 1, { [0] = "Off" } ),
    ["desync_mode"] = ui.new_checkbox( "AA", "Anti-aimbot angles", "Custom desync" ), 
    ["jitter"] = ui.new_checkbox( "AA", "Anti-aimbot angles", "Anti-aim jitter" ),
    ["jitter_mode"] = ui.new_combobox( "AA", "Anti-aimbot angles", "Jitter mode", { "Opposite", "Random" } ),
    ["jitter_chance"] = ui.new_slider( "AA", "Anti-aimbot angles", "Jitter chance", 0, 100, 20, true, "%" ),
    --["anti_brute"] = ui.new_checkbox( "AA", "Anti-aimbot angles", "Anti-Brute" ),
    --["anti_brute_combo"] = ui.new_combobox( "AA", "Anti-aimbot angles", "Anti-bruteforce mode", { "Opposite", "Random", "Step" } ),
    --["anti_brute_range"] = ui.new_slider( "AA", "Anti-aimbot angles", "Anti-bruteforce range", 1, 100, 32 ),
    ["manualaashit"] = {
        ["manual_enable"] = ui.new_checkbox( "AA", "Anti-aimbot angles", "Manual AA Toggle" ),
        ["left"] = ui.new_hotkey( "AA", "Anti-aimbot angles", "Manual Left" ),
        ["right"] = ui.new_hotkey( "AA", "Anti-aimbot angles", "Manual Right" ),
        ["back"] = ui.new_hotkey( "AA", "Anti-aimbot angles", "Manual Backward" ),
        ["manualaa"] = ui.new_checkbox( "AA", "Anti-aimbot angles", "Manual AA Arrows" ),
    },
}
local aim = {
    ["label"] = ui.new_label( "Rage", "Other", "AllStars helpers"),
    ["fallback"] = {
        ["enable"] = ui.new_checkbox( "Rage", "Other", "Enable fallback"),
        ["doubletap_fallback"] = ui.new_multiselect( "Rage", "Other", "DT Fallback", { "Prefer safe point", "Force safe point", "Force body aim" } ),
        ["missedshots"] = ui.new_slider( "Rage", "Other", "After 'x' shots", 1, 10, 5, true, "x" ),
        ["shots"] = ui.new_multiselect( "Rage", "Other", "After 'x' shots modes", { "Prefer safe point", "Force safe point", "Force body aim", "Delay shot" } ),  
    },
}

local vis = {
    ["indicator"] = {
        ["enable"] = ui.new_checkbox( "Visuals", "Other ESP", "AllStars indicators" ),
        ["indicators_modes"] = ui.new_multiselect( "Visuals", "Other ESP", "Options", { "Watermark", "Indicators", "Keybinds", "Spectator" } ),
        ["options"] = ui.new_multiselect( "Visuals", "Other ESP", "Indicators",  { "Desync", "Choke", "Doubletap Charge", "Ping", "Stand height" } ),
        ["hotkey"] = ui.new_multiselect( "Visuals", "Other ESP", "Hotkeys", { "Double tap", "On shot anti-aim", "Fakeduck", "Slow motion", "Prefer safe point", "Force safe point", "Force body aim", "Delay shots" } ),
        ["color_label"] = ui.new_label( "Visuals", "Other ESP", "Accent color" ),
        ["color_picker"] = ui.new_color_picker( "Visuals", "Other ESP", "Color picker 2", 187, 130, 250, 255 ),
        ["logs"] = ui.new_checkbox( "Visuals", "Other ESP", "Logs" ),
        ["logs_duration"] = ui.new_slider( "Visuals", "Other ESP", "Log duration", 1, 5, 2, true, "%" ),
    },
    ["disable_collision"] = ui.new_checkbox( "Visuals", "Other ESP", "Disable Thirdperson Collision" ),
    --["clantag_enable"] = ui.new_checkbox( "Visuals", "Other ESP", "Clantag" ),
}

local function Menu()
    local stateAA = includes( ui.get( Blackboy ), "Anti-Aim" )
    ui.set_visible( aa.base, stateAA )
    ui.set_visible( aa.desync_mode, stateAA )
    ui.set_visible( aa.lean_left, stateAA )
    ui.set_visible( aa.lean_right, stateAA )
    ui.set_visible( aa.delta, stateAA )
    ui.set_visible( aa.jitter, stateAA )
    ui.set_visible( aa.jitter_mode, stateAA and ui.get( aa.jitter ) )
    ui.set_visible( aa.jitter_chance, stateAA and ui.get( aa.jitter ) ) 
    --ui.set_visible( aa.anti_brute, stateAA )
    --ui.set_visible( aa.anti_brute_combo, stateAA and ui.get( aa.anti_brute ) )
    --ui.set_visible( aa.anti_brute_range, stateAA and ui.get( aa.anti_brute ) )
    ui.set_visible( aa.manualaashit.manual_enable, stateAA )
    ui.set_visible( aa.manualaashit.left, stateAA and ui.get( aa.manualaashit.manual_enable ))
    ui.set_visible( aa.manualaashit.right, stateAA and ui.get( aa.manualaashit.manual_enable ))
    ui.set_visible( aa.manualaashit.back, stateAA and ui.get( aa.manualaashit.manual_enable ))
    ui.set_visible( aa.manualaashit.manualaa, stateAA and ui.get( aa.manualaashit.manual_enable ) )

    local stateAIM = includes( ui.get( Blackboy ), "Aimbot" )
    ui.set_visible( aim.label, stateAIM )

    ui.set_visible( aim.fallback.enable, stateAIM )
    ui.set_visible( aim.fallback.doubletap_fallback, stateAIM and ui.get( aim.fallback.enable ) )
    ui.set_visible( aim.fallback.missedshots, stateAIM and ui.get( aim.fallback.enable ) )
    ui.set_visible( aim.fallback.shots, stateAIM and ui.get( aim.fallback.enable ) )

    local stateVIS = includes( ui.get( Blackboy ), "Visuals" )
    ui.set_visible( vis.indicator.enable, stateVIS )
    ui.set_visible( vis.indicator.indicators_modes, stateVIS and ui.get( vis.indicator.enable ) )
    ui.set_visible( vis.indicator.options, stateVIS and ui.get( vis.indicator.enable ) )
    ui.set_visible( vis.indicator.hotkey, stateVIS and ui.get( vis.indicator.enable ) )
    ui.set_visible( vis.indicator.color_label, stateVIS and ui.get( vis.indicator.enable ) )
    ui.set_visible( vis.indicator.color_picker, stateVIS and ui.get( vis.indicator.enable ) )
    ui.set_visible( vis.indicator.logs, stateVIS and ui.get( vis.indicator.enable ) )
    ui.set_visible( vis.indicator.logs_duration, stateVIS and ui.get( vis.indicator.logs ) )
    ui.set_visible( vis.disable_collision, stateVIS )
    --ui.set_visible( vis.clantag_enable, stateVIS )
end 

--For aa
local ChokedLimit = 0
local LastChoke = 0
local CurrentChoke = 0
local SendPacket = false
local AllowSend = false

local function normalize_yaw(angle)
	angle = (angle % 360 + 360) % 360
	return angle > 180 and angle - 360 or angle
end

local function round(num, numDecimalPlaces)
	local mult = 10^(numDecimalPlaces or 0)
	return math.floor(num * mult + 0.5) / mult
end

local ft_prev = 0
local function get_fps()
	ft_prev = ft_prev * 0.9 + globals.absoluteframetime() * 0.1
	return round(1 / ft_prev)
end

--not mine hehe
local function CalcAngle(x, y, ex, ey)
    local delta = { x - ex, y - ey }
	local yaw = math.atan( delta[2] / delta[1] )
	yaw = normalize_yaw( yaw * 180 / math.pi )
	if delta[1] >= 0 then
		yaw = normalize_yaw( yaw + 180 )
	end
	return yaw
end

local function GetClosestPoint(A, B, P)
    local a_to_p = { P[1] - A[1], P[2] - A[2] }
    local a_to_b = { B[1] - A[1], B[2] - A[2] }

    local atb2 = a_to_b[1]^2 + a_to_b[2]^2

    local atp_dot_atb = a_to_p[1]*a_to_b[1] + a_to_p[2]*a_to_b[2]
    local t = atp_dot_atb / atb2
    
    return { A[1] + a_to_b[1]*t, A[2] + a_to_b[2]*t }
end

--[[local should_swap = false --fix this 
local it = 0
local angles = { 60, 20, -60 }
client.set_event_callback( "bullet_impact", function( c )
    if ui.get( aa.anti_brute ) and entity.is_alive( entity.get_local_player( ) ) then
        local ent = client.userid_to_entindex( c.userid )
        if not entity.is_dormant( ent ) and entity.is_enemy( ent ) then
            local ent_shoot = { entity.get_prop(ent, "m_vecOrigin") }
            ent_shoot[3] = ent_shoot[3] + entity.get_prop(ent, "m_vecViewOffset[2]")
            local player_head = { entity.hitbox_position( entity.get_local_player( ), 0 ) }
            local closest = GetClosestPoint( ent_shoot, { c.x, c.y, c.z }, player_head )
            local delta = { player_head[1]-closest[1], player_head[2]-closest[2] }
            local delta_2d = math.sqrt( delta[1] ^ 2 + delta[2] ^ 2 )
        
            if math.abs( delta_2d ) < ui.get( aa.anti_brute_range ) then
                it = it + 1
                should_swap = true
            end
        end
    end
end )

client.set_event_callback( "run_command", function( c )
    if ui.get( aa.anti_brute ) and should_swap then
        local _combo = ui.get( aa.anti_brute_combo )
        if _combo == "Opposite" then
            ui.set( Body[2], -ui.get( Body ) )
        elseif _combo == "Random" then
            ui.set( Body[2], math.random( -60, 60 ) )
        elseif _combo == "Step" then
            ui.set( Body[2], angles[ ( it%3 )+1 ] )
        end
        should_swap = false
    end
end )--]]

local function Angle( x, y )
	local sy = math.sin( math.rad( y ) )
	local cy = math.cos( math.rad( y ) )
	local sp = math.sin( math.rad( x ) )
	local cp = math.cos( math.rad( x ) )
	return cp * cy, cp * sy, -sp
end

local fside = false
--Freestanding body yaw
local ReturnSide = false

local function Freestanding( ent )

    local lp = entity.get_local_player()

    if lp == nil or entity.is_alive( lp ) == false then
        return ReturnSide
    end

    if ent == nil then
        return ReturnSide
    end

    local LocalPos = { entity.get_origin( lp ) }
    local EnemyHeadPos = { entity.hitbox_position( ent, 1 ) } 

    if EnemyHeadPos[1] == nil then
        return
    end

    local YawDif = CalcAngle( LocalPos[1], LocalPos[2], EnemyHeadPos[1], EnemyHeadPos[2] ) 
    local Range = 100 

    local RightDirection = { Angle(0, ( YawDif + 90 ) ) }
    local RightEnd = { 
        LocalPos[1] + RightDirection[1] * Range,
        LocalPos[2] + RightDirection[2] * Range,
        LocalPos[3] + 60,
    }

    local LeftDirection = { Angle(0, ( YawDif - 90 ) ) }
    local LeftEnd = { 
        LocalPos[1] + LeftDirection[1] * Range,
        LocalPos[2] + LeftDirection[2] * Range,
        LocalPos[3] + 60,
    }

    local _, LeftDMG = client.trace_bullet( ent, EnemyHeadPos[1], EnemyHeadPos[2], EnemyHeadPos[3], LeftEnd[1], LeftEnd[2], LeftEnd[3] )
    local _, RightDMG = client.trace_bullet( ent, EnemyHeadPos[1], EnemyHeadPos[2], EnemyHeadPos[3], RightEnd[1], RightEnd[2], RightEnd[3] )

    if LeftDMG > RightDMG then
        ReturnSide = false
    elseif RightDMG > LeftDMG then
        ReturnSide = true
    end
    return ReturnSide
end

local degtimer = 0

local function SetupCommand( Cmd )

    if Cmd.chokedcommands < LastChoke then --sent
        ChokedLimit = LastChoke
    end
    LastChoke = Cmd.chokedcommands

    CurrentChoke = Cmd.chokedcommands

    if CurrentChoke >= ChokedLimit then
        SendPacket = true
        CurrentChoke = 0
    else
        SendPacket = false
    end

    if includes( ui.get( Blackboy ), "Anti-Aim" ) then
        Cmd.allow_send_packet = Cmd.chokedcommands >= ChokedLimit
    end

    AllowSend = Cmd.allow_send_packet
    local GameOftheRules = entity.get_game_rules()
    local Freezetime = entity.get_prop( GameOftheRules , "m_bFreezePeriod" ) == 1
    local weapon = entity.get_player_weapon(entity.get_local_player())
    if not Freezetime then
        if weapon ~= nil and entity.get_classname( weapon ) == 'CC4' then
            if Cmd.in_attack == 1 then
                Cmd.in_attack = 0
                Cmd.in_use = 1
            end
        else
            if Cmd.in_use == 1 then
                if Cmd.chokedcommands == 2 then
                    Cmd.in_use = 0 
                end
            end
        end
    end
end

local target = 0 
local distance = 0

local function GetDist( from, to )
    local FromOrigin = Vector3(entity.get_prop(from, "m_vecOrigin"))
    local ToOrigin = Vector3(entity.get_prop(to, "m_vecOrigin"))
    local dist = FromOrigin:dist_to( ToOrigin )
    if dist ~= nil then
        return ( ( dist * 0.0254 ) * 3.281 )
    end
    return nil
end

local function getNearestEnemy()
	local enemy_players = entity.get_players(true)
	if #enemy_players ~= 0 then
		local own_x, own_y, own_z = client.eye_position()
		local own_pitch, own_yaw = client.camera_angles()
		local closest_enemy = nil
		local closest_distance = 999999999
		        
		for i = 1, #enemy_players do
			local enemy = enemy_players[i]
			local enemy_x, enemy_y, enemy_z = entity.hitbox_position(enemy, 0)
		            
			local x = enemy_x - own_x
			local y = enemy_y - own_y
			local z = enemy_z - own_z 

			local yaw = ((math.atan2(y, x) * 180 / math.pi))
			local pitch = -(math.atan2(z, math.sqrt(math.pow(x, 2) + math.pow(y, 2))) * 180 / math.pi)

			local yaw_dif = math.abs(own_yaw % 360 - yaw % 360) % 360
			local pitch_dif = math.abs(own_pitch - pitch ) % 360
	            
			if yaw_dif > 180 then yaw_dif = 360 - yaw_dif end
			local real_dif = math.sqrt(math.pow(yaw_dif, 2) + math.pow(pitch_dif, 2))

			if closest_distance > real_dif then
				closest_distance = real_dif
				closest_enemy = enemy
			end
		end

		if closest_enemy ~= nil then
			return closest_enemy, closest_distance
		end
	end

	return nil, nil
end


local function IsVisible( ent )
    if ent == nil then
        return false
    end
    for i=1, 18 do
        local pos = { entity.hitbox_position( ent, i ) }
        local lpos = { entity.hitbox_position( entity.get_local_player(), 0 ) }
        if client.visible( pos[1], pos[2], pos[3] ) then
            return true
        end
    end
    return false
end

local function paint()
    local tar, _ = getNearestEnemy()
    target = tar
    distance = GetDist( entity.get_local_player(), target )
end

local players = { 
    ent = {},
    missed = {},
    side = {}, -- 0 == auto, 1 == left, 2 == right
    LastChoke = {},
    ChokeLimit = {},
}
--Handles when they have either shot at us or hit us
local function GetClosestPoint(A, B, P)
    local a_to_p = { P[1] - A[1], P[2] - A[2] }
    local a_to_b = { B[1] - A[1], B[2] - A[2] }

    local atb2 = a_to_b[1]^2 + a_to_b[2]^2

    local atp_dot_atb = a_to_p[1]*a_to_b[1] + a_to_p[2]*a_to_b[2]
    local t = atp_dot_atb / atb2
    
    return { A[1] + a_to_b[1]*t, A[2] + a_to_b[2]*t }
end

local function Within( val, val2, range )
    return math.abs( val-val2 ) <= range
end

local function time_to_ticks(input)
    return 0.5 + input / globals.tickinterval()
end    

local function getchokedticks(ent)    
    local simtime = entity.get_prop(ent,"m_flSimulationTime")
    local simdif = globals.curtime() - simtime
    local latency = client.latency()
    local choke = time_to_ticks(math.max(0.0, simdif - latency))
    return math.floor( choke )
end

local side = 0
local bSide = false
local left, right, back = false, false, true
local bJitter = false
local function RunCmd( Cmd ) 
    local enemies = entity.get_players( true )
    for i=1, #enemies do 
        local player = enemies[i]
        if not includes( players.ent, player ) then
            table.insert( players.ent, player )
            table.insert( players.missed, 0 )
            table.insert( players.LastChoke, 0 )
            table.insert( players.ChokeLimit, 0 )
        end
    end

    for i=1, #players.ent do
        local player = players.ent[i]
        if not entity.is_enemy( player ) then
            table.remove( players.ent, i )
            table.remove( players.missed, i )
            table.remove( players.LastChoke, i )
            table.remove( players.ChokeLimit, i )
        end

        if not entity.is_alive( player ) then
            table.remove( players.ent, i )
            table.remove( players.missed, i )
            table.remove( players.LastChoke, i )
            table.remove( players.ChokeLimit, i )
        end
        if not entity.is_alive( player ) then
            table.remove( players.ent, i )
            table.remove( players.missed, i )
            table.remove( players.LastChoke, i )
            table.remove( players.ChokeLimit, i )
        end

        if entity.get_player_name( player ) == "unknown" then
            table.remove( players.ent, i )
            table.remove( players.missed, i )
            table.remove( players.LastChoke, i )
            table.remove( players.ChokeLimit, i )
        end
    end

    for i=1, #players.ent do
        local player = players.ent[i]
        if getchokedticks( player ) < players.LastChoke[i] then
            players.ChokeLimit[i] = players.LastChoke[i]
        end
        players.LastChoke[i] = getchokedticks( player )
    end 

    if includes( ui.get( Blackboy ), "Anti-Aim" ) then
        for i=1, #players.ent do
            local player = players.ent[i]
            if player == target then
                local currnetside = players.side[i]
                side = currnetside
            end
        end

        if ui.get( aa.base ) == "Freestanding" then
            bSide = Freestanding( target )
        elseif ui.get( aa.base ) == "Reversed freestanding" then
            bSide = not Freestanding( target )
        else
            if SendPacket then
                bJitter = not bJitter
            end
            bSide = bJitter
        end


        if ui.get( aa.desync_mode ) then
            ui.set( Body[1], "Opposite" )
            ui.set( FakeYawLimit, 0 )

        else
            ui.set( Body[1], "Static" )
        end

        local WantedYaw = 0

        if ui.get( aa.manualaashit.left ) then
            left = true
            right = false
            back = false
        elseif ui.get( aa.manualaashit.right ) then
            left = false
            right = true
            back = false
        elseif ui.get( aa.manualaashit.back ) then
            left = false
            right = false
            back = true
        end
        
        if ui.get( aa.manualaashit.manual_enable ) then
            if left then
                WantedYaw = -90
            elseif right then
                WantedYaw = 90
            elseif forawrd then
                WantedYaw = 180
            else
                WantedYaw = 0
            end
        else 
            if left then
                WantedYaw = 0
            elseif right then
                WantedYaw = 0
            elseif forawrd then
                WantedYaw = 0
            else
                WantedYaw = 0
            end
        end

        local shouldaa = not ( ui.get( fakepeek[1] ) and ui.get( fakepeek[2] ) )

        local delta = ui.get( aa.delta)
        if delta == 61 then
            delta = 120
        end

        local lean = { math.floor( ui.get( aa.lean_left ) / 1.66666666667 ), math.floor( ui.get( aa.lean_right ) / 1.66666666667 ) }

        if ui.get( aa.desync_mode ) then
            if shouldaa then
                if SendPacket then
                    if bSide then
                        WantedYaw = WantedYaw - lean[1]
                    else
                        WantedYaw = WantedYaw + lean[2]
                    end
                else
                    if bSide then
                        WantedYaw = WantedYaw + ( math.floor( -lean[1] ) + delta )
                    else
                        WantedYaw = WantedYaw + ( math.floor( lean[2] ) - delta )
                    end
                end
            else
                if bSide then
                    WantedYaw = WantedYaw - lean[1]
                else
                    WantedYaw = WantedYaw + lean[2]
                end
            end
        else
            if bSide then
                ui.set( Body[2], -delta )
                WantedYaw = WantedYaw - lean[1]
            else
                ui.set( Body[2], delta )
                WantedYaw = WantedYaw + lean[2]
            end
        end

        if ui.get( aa.jitter ) and shouldaa then
            local shoulddistort = math.random( 0, 100 ) < ui.get( aa.jitter_chance ) and not SendPacket
            local mode = ui.get( aa.jitter_mode ) 
            if ui.get( aa.desync_mode ) then
                if shoulddistort then
                    if mode == "Opposite" then
                        if bSide then
                            WantedYaw = ( math.floor( -lean[1] ) - delta )
                        else
                            WantedYaw = ( math.floor( lean[2] ) + delta )
                        end
                    else
                        WantedYaw = math.random( -delta, delta )
                    end
                end
            else
                if shoulddistort then
                    if mode == "Opposite" then
                        if bSide then
                            ui.set( Body[2], delta )
                        else
                            ui.set( Body[2], -delta )
                        end
                    else
                        ui.set( Body[2], math.random( -delta, delta ) )
                    end
                end
            end
        end
        ui.set( Yaw[2], normalize_yaw( WantedYaw ) )
    end

    if includes( ui.get( Blackboy ), "Aimbot" ) then
        if ui.get( aim.fallback.enable ) then
            if includes( ui.get( aim.fallback.doubletap_fallback ), "Prefer safe point" ) or includes( ui.get( aim.fallback.shots ), "Prefer safe point" ) then
                ui.set( PreferSafe, false )
            end
            if includes( ui.get( aim.fallback.doubletap_fallback ), "Force safe point" ) or includes( ui.get( aim.fallback.shots ), "Force safe point" ) then
                ui.set( Forcesafe, "On hotkey" )
            end
            if includes( ui.get( aim.fallback.doubletap_fallback ), "Force body aim" ) or includes( ui.get( aim.fallback.shots ), "Force body aim" ) then
                ui.set( forcebodyaim, "On hotkey" )
            end
            if includes( ui.get( aim.fallback.shots ), "Delay shot" ) then
                ui.set( delayshots, false )
            end

            if ui.get( doubletap[2] ) then
                if includes( ui.get( aim.fallback.doubletap_fallback ), "Prefer safe point" ) then
                    ui.set( PreferSafe, true )
                end
                if includes( ui.get( aim.fallback.doubletap_fallback ), "Force safe point" ) then
                    ui.set( Forcesafe, "Always on" )
                end
                if includes( ui.get( aim.fallback.doubletap_fallback ), "Force body aim" ) then
                    ui.set( forcebodyaim, "Always on" )
                end
            end

            if includes( players.ent, target ) then
                for i=1, #players.ent do 
                    local player = players.ent[i]
                    if player == target then
                        local shots = players.missed[i]
                        if shots >= ui.get( aim.fallback.missedshots ) then
                            if includes( ui.get( aim.fallback.shots ), "Prefer safe point" ) then
                                ui.set( PreferSafe, true )
                            end
                            if includes( ui.get( aim.fallback.shots ), "Force safe point" ) then
                                ui.set( Forcesafe, "Always on" )
                            end
                            if includes( ui.get( aim.fallback.shots ), "Force body aim" ) then
                                ui.set( forcebodyaim, "Always on" )
                            end
                            if includes( ui.get( aim.fallback.shots ), "Delay shot" ) then
                                ui.set( delayshots, true )
                            end
                        end
                    end
                end
            end
        end
    end
end

ui.set_callback( aa.desync_mode, function()
    ui.set( FakeYawLimit, 58)
end)


local function GetFakeLagInfo( ent )
    local Choke = 0
    local Limit = 0
    for i=1, #players.ent do
        local player = players.ent[i]
        if player == ent then
            Choke = getchokedticks( ent )
            Limit = players.ChokeLimit[i]
            break
        end
    end
    return Choke, Limit
end

local hits = 0
local misses = 0

local function DrawContainer( x, y, w, h, color, color2, dir, textalpha )
    renderer.rectangle(x, y, w, h, 11, 11, 11, textalpha, dir ) -- Main Box
    renderer.rectangle(x, y, 2, h, 11, 11, 11, textalpha) -- Left Line
    renderer.rectangle(x + w, y, 2, h + 2, 11, 11, 11, textalpha) -- Right Line
    renderer.rectangle(x, y, w, 2, 11, 11, 11, textalpha, dir ) -- Bottom Line
    renderer.rectangle(x + 2, y + 2, w - 2, 2, color[1], color[2], color[3], textalpha, dir ) -- Stupid Line
    renderer.rectangle(x, y + h, w, 2, 11, 11, 11, textalpha, dir ) -- Top Line
end

local function DrawLog( x, y, text, color ,color2, b, talpha )
    local TextWH = { renderer.measure_text( nil, b ) }
    TextWH[1] = TextWH[1]*2
    TextWH[2] = TextWH[2]
    local tx = { renderer.measure_text( nil, text) }
    DrawContainer( x, y, tx[1] + 8, tx[2] + 4, color, color2, true, talpha )
    renderer.text( x + 4, y + 4, 255, 255, 255, talpha, nil, 0, text )
end

local logs = {
    text = {},
    hit = {},
    time = {},
    alpha = {},
    textalpha = {},
}

local MaxAlpha = 255

local function AddLog( text, hit, time, alpha, textalpha )
    table.insert( logs.text, text )
    table.insert( logs.hit, hit )
    table.insert( logs.time, time )
    table.insert( logs.alpha, MaxAlpha )
    table.insert( logs.textalpha, textalpha )
end

local hitgroup_names = { "body", "head", "chest", "stomach", "left arm", "right arm", "left leg", "right leg", "neck", "?", "gear" }
local lastid = 0
local function AimMiss( e )
    if lastid == e.id then
        return
    end
    lastid = e.id
    misses = misses + 1
    local target = e.target
    if e.reason == "?" then
        for i=1, #players.ent do    
            local player = players.ent[i]
            if player == target then
                players.missed[i] = players.missed[i] + 1
            end
        end
    end

    if ui.get( vis.indicator.logs ) then
        local choke, limit = GetFakeLagInfo( e.target )
        local dt = limit <= 3
        local hc = math.floor( e.hit_chance + 0.5 )    .. "%"
        local name = entity.get_player_name(e.target)
        local hb = hitgroup_names[ e.hitgroup + 1 ]
        local reason 
        if e.reason == "?" then
            reason = "bad resolve"
        else
            reason = e.reason
        end

        local string = string.format( "Missed %s due to %s | hitchance: %s | hitbox: %s | Doubletap: %s | Choke: %d", name, reason, hc, hb, dt, limit  )
        print( string )
        AddLog( string, "MISS", globals.curtime() + ui.get( vis.indicator.logs_duration ), 140, 255 )
    end
end

local weaponother = {
    hegrenade = "Naded",
    knife = "Knifed",
    inferno = "Burned",
    taser = "Tased",
}

local function PlayerHurt( e )
    if client.userid_to_entindex( e.attacker ) ~= entity.get_local_player() then
        return
    end

    hits = hits + 1

    if ui.get( vis.indicator.logs ) then
        local target = client.userid_to_entindex( e.userid )
        local name = entity.get_player_name( target )
        local dmg = e.dmg_health
        local healthleft = e.health
        local hitbox = hitgroup_names[ e.hitgroup + 1 ] 
        local nigga = "Hit"

        if hitbox == "body" then
            if weaponother[e.weapon] ~= nil then
                nigga = weaponother[e.weapon]
            else
                nigga = "Hit"
            end
        end
    
        local string = string.format( "%s %s in the %s for %d (%d health remaining)", nigga, name, hitbox, dmg, healthleft )
        print( string )
        AddLog( string, "HIT", globals.curtime() + ui.get( vis.indicator.logs_duration ), 140, 255 )
    end
end

local logweapon = ui.reference( "MISC", "Miscellaneous", "Log weapon purchases" ) 
local logdmg = ui.reference( "MISC", "Miscellaneous", "Log damage dealt" ) 

local function RenderLogs()
    if not ui.get( vis.indicator.logs ) then
        for i=1, #logs.text do
            table.remove( logs.text, i )
            table.remove( logs.hit, i )
            table.remove( logs.time, i )
            table.remove( logs.alpha, i )
            table.remove( logs.textalpha, i )
        end
        return
    end

    ui.set( logweapon, false )
    ui.set( logdmg, false )

    local color = { ui.get( vis.indicator.color_picker ) }

    local offset = 0
    for i=1, #logs.text do
        if logs.text[i] == nil then
            return
        end
        local text = logs.text[i]
        local hit = logs.hit[i]
        local time = logs.time[i]
        local alpha = logs.alpha[i]
        local textalpha = logs.textalpha[i]

        if time < globals.curtime() then
            logs.alpha[i] = alpha - ( MaxAlpha / 0.6 ) * globals.frametime()
            logs.textalpha[i] = alpha - ( 255 / 0.6 ) * globals.frametime()
        end

        logs.alpha[i] = math.min( 140, math.max( 0, logs.alpha[i] ) )
        logs.textalpha[i] = math.min( 255, math.max( 0, logs.textalpha[i] ) )

        if alpha > 0 then
            DrawLog( 10, 10 + offset, text, { color[1], color[2], color[3], logs.alpha[i] }, { color[1], color[2], color[3], logs.alpha[i] }, hit, textalpha )
            offset = offset + 25
        else
            table.remove( logs.text, i )
            table.remove( logs.hit, i )
            table.remove( logs.time, i )
            table.remove( logs.alpha, i )
            table.remove( logs.textalpha, i )
        end
    end
end

local function lerp(a, b, percentage)
	return a + (b - a) * percentage
end

local function table_lerp(a, b, percentage)
	local result = {}
	for i=1, #a do
		result[i] = lerp(a[i], b[i], percentage)
	end
	return result
end


--Indicators
local function DrawBar( x, y, width, height, percent, color, color2, background )
    percent = math.max( 0.1, percent )
    
    renderer.rectangle( x, y, width, height, background[1], background[2], background[3], background[4] )
    local Width2 = math.min( width, percent*width )
    renderer.rectangle( x + 2, y + 2, Width2 - 4, height - 4, color[1], color[2], color[3], color[4], true )
end

--[[local clantagisset = 0
local function clantaggyboy()
    if ui.get( vis.clantag_enable ) and not clantagisset == 1 then
        client.set_clan_tag("Black Test")
    else
        client.set_clan_tag("")
    end
end--]]

local function GetHotkeyState( key )
    local names={
        [0]='Always on',
        [1]='On hotkey',
        [2]='Toggle',
        [3]='Off hotkey'
    }
    local k = { ui.get( key ) }
    return names[k[2]]
end

local function DrawHotkeyState( x, y, w, hotkey, state )
    state = GetHotkeyState( state )
    state = string.format( "[%s]", state )
    local text2 = { renderer.measure_text( "", state ) }
    renderer.text( x + 5, y, 255, 255, 255, 255, "", 0, hotkey )
    renderer.text( x + w - text2[1] - 5, y, 255, 255, 255, 255, "", 0, state )
end

local function DrawBox( x, y, w, h, color, color2, name )
    renderer.rectangle(x + 2, y, w - 2, h + 2, 0, 0, 0, 130, true ) -- Main Box
    renderer.rectangle(x, y, 2, h + 2, 0, 0, 0, 130) -- Left Line
    renderer.rectangle(x + w, y, 2, h + 2, 0, 0, 0, 130) -- Right Line

    local _,h2 = renderer.measure_text("c", name )
    h2 = h2 + 5
    local alpha = 150

    renderer.rectangle(x, y - 2, w, h2 + 2, 11, 11, 11, 255, true ) -- Top Box
    renderer.rectangle(x, y - 2, 2, h2 + 2, 11, 11, 11, 255) -- Weird Top Left
    renderer.rectangle(x + w, y - 2, 2, h2 + 3, 11, 11, 11, 255) -- Weird Top Right
    renderer.rectangle(x + 2, y, w - 2, 2, color[1], color[2], color[3], 255, true) -- Stupid Line
    renderer.rectangle(x, y + 3, w, 2, 11, 11, 11, 255, true ) -- Top
    renderer.rectangle(x, y + h2, w + 2, 2, 11, 11, 11, 255, true ) -- Bottom
    renderer.text(x + (w/2), y + h2/2 + 2, 255, 255, 255, 255, "c", 0, name )
end

local IndicatorDrag = false
local IndicatorXDrag = 0
local IndicatorYDrag = 0

local HotkeyDrag = false
local HotkeyXDrag = 0
local HotkeyYDrag = 0

local SpecDrag = false
local SpecXDrag = 0
local SpecYDrag = 0

local Saved = {
    ["IndicatorsX"] = ui.new_string( "Blackboy:INDICATORX", 20 ),
    ["IndicatorsY"] = ui.new_string( "Blackboy:INDICATORY", 545 ),
    ["HotkeyXDrag"] = ui.new_string( "Blackboy:HotkeyXDrag", 20 ),
    ["HotkeyYDrag"] = ui.new_string( "Blackboy:HotkeyYDrag", 615 ),
    ["SpecX"] = ui.new_string( "Blackboy:SPECX", 1735 ),
    ["SpecY"] = ui.new_string( "Blackboy:SPECY", 450 ),
}

local maxwidth = 0
local function IndicatorFunc( )
    local lp = entity.get_local_player
    if lp() == nil then return end
    if entity.is_alive( lp() ) == false then return end
    if includes( ui.get( Blackboy ), "Visuals" ) == false then return end

    if ui.get( vis.disable_collision ) then 
        client.exec( "cam_collision 0" )
    else
        client.exec( "cam_collision 1" )
    end

    local screensizemen = { client.screen_size( ) }
    local center = { screensizemen[1]/2, screensizemen[2]/2 } 

    if ui.get( aa.manualaashit.manualaa ) then
        local color = { ui.get( vis.indicator.color_picker ) }
        
        --Shadows
        renderer.text( center[1] - 45, center[2] - 3, 11, 11, 11, 85, "cb+", 0, "⯇" )
        renderer.text( center[1], center[2] + 45, 11, 11, 11, 85, "cb+", 0, "⯆" )
        renderer.text( center[1] + 45, center[2] - 3, 11, 11, 11, 85, "cb+", 0, "⯈" )

        --Arrows
        if left == true then
            renderer.text( center[1] - 45, center[2] - 3, color[1], color[2], color[3], 255, "cb+", 0, "⯇" )

        elseif back == true then
            renderer.text( center[1], center[2] + 45, color[1], color[2], color[3], 255, "cb+", 0, "⯆" )

        elseif right == true then
            renderer.text( center[1] + 45, center[2] - 3, color[1], color[2], color[3], 255, "cb+", 0, "⯈" )

        end
    end
    
    if ui.get( vis.indicator.enable ) == false then return end 

    if includes( ui.get( vis.indicator.indicators_modes ), "Indicators" ) then
        for i = 8000, 1, -1 do
            renderer.indicator(255, 255, 255, 0, i)
        end
    end

    if includes( ui.get( vis.indicator.indicators_modes ), "Indicators" ) then
        local x, y = ui.get( Saved.IndicatorsX ), ui.get( Saved.IndicatorsY )
        local w, h = 160, 18
        local mx,my = ui.mouse_position()

        if ui.is_menu_open() then
            if IndicatorDrag and not client.key_state(0x01) then
                IndicatorDrag = false
            end

            if IndicatorDrag and client.key_state(0x01) then
                x = mx - IndicatorXDrag
                y = my - IndicatorYDrag
                ui.set( Saved.IndicatorsX , x )
                ui.set( Saved.IndicatorsY , y )
            end

            if mx > x - 5 and mx < x + w + 5 and my > y - 5 and my < y + h + 5 and client.key_state(0x01) then
                IndicatorDrag = true
                IndicatorXDrag = mx - x
                IndicatorYDrag = my - y
            end
        end

        local items = {
            name = {},
            value = {},
        }

        local function AddItem( name, value )
            table.insert( items.name, name )
            table.insert( items.value, value )
        end

        local color = { ui.get( vis.indicator.color_picker ) }

        local lby = entity.get_prop( lp() , "m_flLowerBodyYawTarget" )
        local yaw = math.max(-60, math.min(60, math.floor((entity.get_prop( lp(), "m_flPoseParameter", 11) or 0)*120-60+0.5)))
        local _, body = entity.get_prop( lp() , "m_angAbsRotation" )

        local vel_x, vel_y = entity.get_prop(entity.get_local_player(), "m_vecVelocity")
        local vel_real = math.floor(math.min(10000, math.sqrt(vel_x * vel_x + vel_y * vel_y) + 0.5))
        local dif = math.abs( normalize_yaw( lby - body ) ) 

        dif = math.min( 58, dif )
        
        if includes( ui.get( vis.indicator.options ), "Desync" ) then
            AddItem( "Desync", dif/58 )
        end

        if includes( ui.get( vis.indicator.options ), "Choke" ) then
            AddItem( "Choke", globals.chokedcommands() / (ui.get( maxusr) - 2) --[[ <= Get max fakelag ]] )
        end

        if includes( ui.get( vis.indicator.options ), "Doubletap Charge" ) then

        end

        if includes( ui.get( vis.indicator.options ), "Ping" ) then
            local latency = math.floor( math.min( 1000, client.latency()*1000 ) + 0.5 )

            AddItem( "Ping", latency / 100 )
        end

        if includes( ui.get( vis.indicator.options ), "Stand height" ) then
            local local_player = entity.get_local_player()
            local head = { entity.hitbox_position( local_player, "head_0" ) }
            local pelvis = { entity.hitbox_position( local_player, "pelvis" ) }
            local difference = head[3] - pelvis[3]
            difference = ( math.floor( difference ) / 30 )
            difference = math.min( 1, math.max( 0, difference ) )
            AddItem( "Stand height", difference )
        end

        if includes( ui.get( vis.indicator.options ), "Accuracy" ) then
            if hits + misses ~= 0 then
                AddItem( "Accuracy", hits / (hits + misses) )
            else
                AddItem( "Accuracy", 1 )
            end
        end

        local widest = 0
        local offset = 21

        for i=1, #items.name do 
            local name = items.name[i]
            h = h + 14.5
            local tx = { renderer.measure_text(nil, name ) }
            if tx[1] > widest then
                widest = tx[1]
            end
            maxwidth = 148 - widest
        end

        DrawBox( x, y, w, h, color, color2, "Indicators")

        for i=1, #items.name do
            local name = items.name[i]
            local val = items.value[i]
            renderer.text( x + 5, y + offset - 2, 255, 255, 255, 255, "", 0, name )
            local tx = { renderer.measure_text(nil, name ) }
            DrawBar( x + 9 + widest, y + offset, maxwidth, 10, val, {color[1], color[2], color[3], 255}, { color[1], color[2], color[3], 255 }, { 20, 20, 20, 255 }  )
            offset = offset + 14
        end
    end

    if includes( ui.get( vis.indicator.indicators_modes ), "Keybinds") then
        local x, y = ui.get( Saved.HotkeyXDrag ), ui.get( Saved.HotkeyYDrag )
        local w, h = 160, 18
        local mx,my = ui.mouse_position()

        if ui.is_menu_open() then
            if HotkeyDrag and not client.key_state(0x01) then
                HotkeyDrag = false
            end

            if HotkeyDrag and client.key_state(0x01) then
                x = mx - HotkeyXDrag
                y = my - HotkeyYDrag
                ui.set( Saved.HotkeyXDrag , x )
                ui.set( Saved.HotkeyYDrag , y )
            end

            if mx > x - 5 and mx < x + w + 5 and my > y - 5 and my < y + h + 5 and client.key_state(0x01) then
                HotkeyDrag = true
                HotkeyXDrag = mx - x
                HotkeyYDrag = my - y
            end
        end

        local check = {
            name = { },
            state = { },
        }

        local function AddCheck( name, value )
            table.insert( check.name, name )
            table.insert( check.state, value )
        end

        local color = { ui.get( vis.indicator.color_picker ) }

        if includes( ui.get( vis.indicator.hotkey ), "Double tap" ) then
            if ui.get(doubletap[1]) and ui.get(doubletap[2]) then
                AddCheck( "Double tap", doubletap[2] )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "On shot anti-aim" ) then
            if ui.get(onshot[1]) and ui.get(onshot[2]) then
                AddCheck( "On shot anti-aim", onshot[2] )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "Fakeduck" ) then
            if ui.get(fakeduck) then
                AddCheck( "Fakeduck", fakeduck )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "Slow motion" ) then
            if ui.get(slowwalk[1]) and ui.get(slowwalk[2]) then
                AddCheck( "Slow motion", slowwalk[2] )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "Prefer safe point" ) then
            if ui.get( PreferSafe ) then
                AddCheck( "Prefer safe point", PreferSafe )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "Force safe point" ) then
            if ui.get( Forcesafe ) then
                AddCheck( "Force safe point", Forcesafe )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "Force body aim" ) then
            if ui.get( forcebodyaim ) then
                AddCheck( "Force body aim", forcebodyaim )
            end
        end

        if includes( ui.get( vis.indicator.hotkey ), "Delay shots" ) then
            if ui.get( delayshots ) then
                AddCheck( "Delay shots", delayshots )
            end
        end

        local offset = 21

        for i=1, #check.name do
            h = h + 15.5
        end

        DrawBox( x, y, w, h + 2, color, color2, "Keybinds" )

        for i=1, #check.name do
            local name = check.name[i]
            local hotkey = check.state[i]
            DrawHotkeyState( x, y + offset, w, name, hotkey )
            offset = offset + 15
        end
    end

    if includes( ui.get( vis.indicator.indicators_modes ), "Watermark") then
        local screen_width, screen_height = client.screen_size()
        local center_x, center_y = screen_width / 2, screen_height / 2    
        local color = { ui.get( vis.indicator.color_picker ) }
        --local user_name = cvar.name:get_string()
        local ping = math.floor( math.min( 1000, client.latency()*1000 ) + 0.5 )
        local FPS = get_fps()

        local watermark_text = string.format("AllStars v%s | %s | Delay: %d | Framerate: %s", build, --[[user_name, --]]version, ping, FPS )
        local text_width,text_height = renderer.measure_text( "", watermark_text )
        local text_wi,text_he = renderer.measure_text( "","Black Boy" )
        local x,y = screen_width-text_width - 30, 15
        local w,h = text_width + 10, text_height + 5 
        local alpha = 130

        renderer.rectangle(x, y - 2, w, h + 2, 11, 11, 11, 255, true ) -- Main Box
        renderer.rectangle(x, y - 2, 2, h + 2, 11, 11, 11, 255) -- Left Bar
        renderer.rectangle(x + w, y - 2, 2, h + 4, 11, 11, 11,255) -- Right Bar
        renderer.rectangle(x + 2, y, w - 2, 2, color[1], color[2], color[3], 255, true) -- Stupid Line
        renderer.rectangle(x, y - 2, w, 2, 11, 11, 11, 255, true ) -- Top Bar
        renderer.rectangle(x, y + h, w, 2, 11, 11, 11, 255, true ) -- Bottom Bar
        renderer.text(x + 5, y + text_height/2 - 2, 255, 255, 255, 255, "", 0, watermark_text )
    end

    if includes( ui.get( vis.indicator.indicators_modes ), "Spectator" ) then
        local color = { ui.get( vis.indicator.color_picker ) }
        local x, y = ui.get( Saved.SpecX ), ui.get( Saved.SpecY )
        local w, h = 160, 18
        local mx,my = ui.mouse_position()

        if ui.is_menu_open() then
            if SpecDrag and not client.key_state(0x01) then
                SpecDrag = false
            end

            if SpecDrag and client.key_state(0x01) then
                x = mx - SpecXDrag
                y = my - SpecYDrag
                ui.set( Saved.SpecX, x )
                ui.set( Saved.SpecY, y )
            end

            if mx > x - 5 and mx < x + w + 5 and my > y - 5 and my < y + h + 5 and client.key_state(0x01) then
                SpecDrag = true
                SpecXDrag = mx - x
                SpecYDrag = my - y
            end
        end

        local ActiveSpectators = { }

        local players = entity.get_all("CCSPlayer")
        for i=1, #players do 
            local player = players[i]
            local observer_target = entity.get_prop(player, "m_hObserverTarget")
            if observer_target == entity.get_local_player() then
                table.insert( ActiveSpectators, entity.get_player_name( player ) )
            end
        end

        local DrawSpectators = false

        if ui.is_menu_open() then
            DrawSpectators = true
        end

        if #ActiveSpectators > 0 then
            DrawSpectators = true
        end

        if DrawSpectators then
            local offset = 19
            for i=1, #ActiveSpectators do
                h = h + 15
            end
            DrawBox( x, y, w, h, color, color, "Spectators" )
            for i=1, #ActiveSpectators do
                local spec = ActiveSpectators[i]
                renderer.text( x + 5, y + offset, 255, 255, 255, 255, "", 0, spec )
                offset = offset + 15
            end
        end
    end
end

local function client_set_event_callback( event, table )
    for i=1, #table do
        client.set_event_callback( event, table[i] )
    end
end

client_set_event_callback( "aim_miss", { AimMiss } )
client_set_event_callback( "paint_ui", { Menu } )
client_set_event_callback( "paint", { paint, IndicatorFunc, RenderLogs } )
client_set_event_callback( "run_command", { RunCmd } )
client_set_event_callback( "setup_command", { SetupCommand } )
client_set_event_callback( "player_hurt", { PlayerHurt } )
client_set_event_callback( "player_spawn", { run_buybot } )