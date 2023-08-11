//array of tables
::teams <- []
//table of <playerid>=<team number>
::teamsByPlayers <- {}


function moveEveryoneToTeam(iTeam) {
	for (local i = 1; i <= MaxPlayers ; i++)
	{
	    local player = PlayerInstanceFromIndex(i)
	    if (player == null) continue
	    movePlayerToTeam(player, iTeam)
	}	
}

function partitionCTFTeamToCustom(iTeam) {
	if (iTeam > 3) {
		printl("No can do. we only subdivide native teams.")
		return 1
	}
	// iterate throughout players and move those in team `iTeam` to custom team
	::MaxPlayers <- MaxClients().tointeger();
		
	for (local i = 1; i <= MaxPlayers ; i++) {
    local player = PlayerInstanceFromIndex(i)
    if (player == null) continue
    if (player.GetTeam() == iTeam) {
    	if (iPlayersMoved >= iPlayersPerTeam) {
    		iPlayersMoved = 0
    		iCurrentTeam++
    	}
    	movePlayerToTeam(player, 4+iCurrentTeam)
    	iPlayersMoved++
    }
	}	
}


function createCustomTeam() {
	local iNewTeam = teams.len()
	teams.append({
		players = []
	})
	if (mapVars.br_team_highlight == 0)
		teams[iNewTeam].color <- RandomInt(0,255)+" "+RandomInt(0,255)+" "+RandomInt(0,255);
}

::ColorPlayer <- function(player, sColor) {
	player.__KeyValueFromString("rendercolor", sColor)
}

::QueueToCustomTeam <- function(hTargetPlayer) {
	// if the last team is full, create new team
	if ((teams.len() == 0) || (teams[teams.len() - 1].players.len() >= iPlayersPerTeam()))
 		createCustomTeam()
	if (!(hTargetPlayer in teamsByPlayers))
		hTargetPlayer.MoveToTeam( 3 + teams.len() )
}

//function TakeOverRecord

::CTFPlayer.MoveToTeam <- function(iTeam) {
	if (iTeam < 4) {
		this.ForceChangeTeam(iTeam, true)
		return
	}
  // move to red
	this.ForceChangeTeam(2, true)
	// add to custom team
	local iCTeam = iTeam-4
	teams[iCTeam].players.append(this);
	teamsByPlayers[this] <- iCTeam
	if (this.GetPlayerClass() == Constants.ETFClass.TF_CLASS_MEDIC)
		teams[iCTeam].hasMedic <- 0
	ColorPlayer(this, teams[iCTeam].color)
	// we remove the cosmetics to ensure new team color
	if (mapVars.vscript_subteam_rid_wearables)
		this.DiscardCosmetics()
}

::CTFBot.MoveToTeam <- CTFPlayer.MoveToTeam

//------------------------------
// Event hooks
//------------------------------


function OnGameEvent_teamplay_round_active(params) {
	//marked for death to every player
	
	for (local i = 1; i <= MaxPlayers ; i++) {
    local player = PlayerInstanceFromIndex(i)
    if (player == null) continue
    player.AddCondEx(30, mapVars.br_marked_time, null)
    player.ClearWeapons()
	}
	//local lsh = GetListenServerHost()
	//lsh.Weapon_Drop(NetProps.GetPropEntityArray(lsh, "m_hMyWeapons", 0))
}

function OnGameEvent_player_disconnect(params) {
	local p = GetPlayerFromUserID(params.userid);
	local idx = teams[teamsByPlayers[p]].players.find(p)
	teams[teamsByPlayers[p]].players.remove(idx)
}

function OnGameEvent_player_death(params) {
	local p = GetPlayerFromUserID(params.userid);
	if (
		(p != null) &&
		("hasMedic" in teams[teamsByPlayers[p]]) &&
		(p.GetPlayerClass() != Constants.ETFClass.TF_CLASS_MEDIC ))
		SpawnReviveMarker(p)
}

function OnGameEvent_player_spawn(params) {
	local p = GetPlayerFromUserID(params.userid);
	if (p != null) {
		if (params.team & 2) {
			QueueToCustomTeam(p)
			hideHUDPlayersTop(p)
		}
	}
}

function OnScriptHook_OnTakeDamage(params) {
/*
	const_entity 	handle 	The entity which took damage
	inflictor 	handle 	The entity which dealt the damage, can be null
	weapon 	handle 	The weapon which dealt the damage, can be null
	attacker 	handle 	The owner of the damage, can be null
	damage 	float 	Damage amount
	max_damage 	float 	Damage cap
	damage_bonus 	float 	Additional damage (e.g. from crits)
	damage_bonus_provider 	handle 	Owner of the damage bonus
	base_damage_const 	float 	Base damage
	damage_force 	Vector 	Damage force
	damage_for_force_calc 	float 	If non-zero, this damage is used for force calculations
	damage_position 	Vector 	Where the damage actually came from
	reported_position 	Vector 	Where the damage supposedly came from
	damage_type 	int 	Damage type. See Constants.FDmgType
	damage_custom 	int 	Special damage type. See Constants.ETFDmgCustom
	damage_stats 	int 	Unused
	force_friendly_fire 	bool 	If true, force the damage to friendlyfire, regardless of this entity's and attacker's team
	ammo_type 	int 	Unused
	player_penetration_count 	int 	How many players the damage has penetrated so far
	damaged_other_players 	int 	How many players other than the attacker has the damage been applied to. Used for rocket jump damage reduction
	crit_type 	int 	Type of crit damage. 0 - None, 1 - Mini, 2 - Full
	early_out 	bool 	If set to true by the script, the game's damage routine will not run and it will simply return the currently set damage. 
*/
	
	//if it's not a player we don't care
	if (!(params.const_entity.IsPlayer() && params.inflictor.IsPlayer()))
		return
	local myTeam = teamsByPlayers[params.const_entity]
	local enemyTeam = teamsByPlayers[params.inflictor]
	if ((myTeam == enemyTeam) && (params.inflictor != params.const_entity))
			params.early_out = true
}
