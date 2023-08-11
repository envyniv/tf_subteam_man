
if (!EventBus) {
	printl("This script requires EventBus!")
	return
}

::Teams <- {
	// these functions need to be overwritten by your script
	iPlayersInTeam <- function() { return 4 }
	iTeamHiglight <- function() { return true }
	bDiscardCosmetics <- function() { return true }
	
	// hPlayer : iTeam
	byPlayers <- {}
	
	/*
		order is per ETFClass
		------------------
		ETFClass
		Name 	Value
		TF_CLASS_UNDEFINED 	0
		TF_CLASS_SCOUT 	1
		TF_CLASS_SNIPER 	2
		TF_CLASS_SOLDIER 	3
		TF_CLASS_DEMOMAN 	4
		TF_CLASS_MEDIC 	5
		TF_CLASS_HEAVYWEAPONS 	6
		TF_CLASS_PYRO 	7
		TF_CLASS_SPY 	8
		TF_CLASS_ENGINEER 	9
		TF_CLASS_CIVILIAN 	10
		TF_CLASS_COUNT_ALL 	11
		TF_CLASS_RANDOM 	12 
	classes : [ 0, 0, 1, 0, 3, 4, 8, 2, 1, 0, 0, 0, 0 ]
	// array of player handles, max is
	players : []
	// this is only added if `bDiscardCosmetics` returns true
	color :
	*/
	list <- []
}

::Teams.bRidWearables <- function() { return true }

::Teams.FindTeamIndex <- function(hPlayer) {
	if (hPlayer in this.byPlayers)
		return this.byPlayers[hPlayer]
	else {
		error(format("player %s is not in a team", hPlayer))
		return null
	}
}

// moves all players to team `iTeam`.
::Teams.ExodusToTeam <- function(iTeam) {
	for (local i = 1; i <= MaxPlayers ; i++)
	{
	    local player = PlayerInstanceFromIndex(i)
	    if (player == null) continue
	    player.MoveToTeam(iTeam)
	}	
}

// partitions (e.g. subdivides) native team into custom teams
// based on `iPlayersInTeam`.
::Teams.partitionCTFTeamToCustom <- function(iTeam) {
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

// Adds new custom team.
::Teams.New() <- function() {
	local iNewTeam = teams.len()
	list.append({
		players = []
	})
	if (mapVars.br_team_highlight == 0)
		list[iNewTeam].color <- RandomInt(0,255)+" "+RandomInt(0,255)+" "+RandomInt(0,255);
}

// Colors player with team color
// ( will be called only if `bDiscardCosmetics` returns true. )
::Teams.ColorPlayer <- function(player) {
	local color = Teams.list[FindTeamIndex(player)].color
	if (color == null)
		return
	player.__KeyValueFromString("rendercolor", color)
}

// Adds a player to a team that's missing a member.
::Teams.QueuePlayer <- function(hTargetPlayer) {
	// if the last team is full, create new team
	if ((list.len() == 0) || (list[list.len() - 1].players.len() >= iPlayersPerTeam()))
 		createCustomTeam()
	if (!(hTargetPlayer in byPlayers))
		hTargetPlayer.MoveToTeam( 3 + teams.len() )
}

::Teams.Remove(hPlayer) {
	local idx = teams[teamsByPlayers[p]].players.find(p)
	teams[teamsByPlayers[p]].players.remove(idx)
}

// Forcibly moves a player to a subteam
::CTFPlayer.MoveToTeam <- function(iTeam) {
	if (iTeam < 4) {
		this.ForceChangeTeam(iTeam, true)
		return
	}
  // move to red
	this.ForceChangeTeam(2, true)
	// add to custom team
	local iCTeam = iTeam-4
	Teams.list[iCTeam].players.append(this);
	// add to quick lookup table
	Teams.byPlayers[this] <- iCTeam
	// increment class counter
	Teams.list[iCTeam].classes[this.GetPlayerClass()]++
	// color player
	Teams.ColorPlayer(this)
	// we remove the cosmetics to ensure new team color
	if (Teams.bRidWearables())
		this.DiscardCosmetics()
}

::CTFBot.MoveToTeam <- CTFPlayer.MoveToTeam

//------------------------------
// Event hooks
//------------------------------

EventBus.Listen("player_disconnect", function(params) {
	local p = GetPlayerFromUserID(params.userid);
	Teams.Remove(p)
}, this)

/*
EventBus.Listen("OnTakeDamage", function(params) {
	local p = GetPlayerFromUserID(params.userid);
	Teams.Remove(p)
}, this)
*/
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
