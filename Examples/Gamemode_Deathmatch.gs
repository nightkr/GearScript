$Deathmatch::Pref::TimeLimit = 5 * 60; // 5 minutes with 60 seconds each = 300 seconds, -1 for infinite
$Deathmatch::Pref::VoteTimeLimit = 15; // Seconds
$Deathmatch::Pref::ScoreLimit = 10; // -1 for infinite
$Deathmatch::Pref::VoteBuildAmount = 5;

forceRequiredAddOn("Server_Permissions");
getPermissionManager().registerPermission("Reset Deathmatch minigame", "deathmatch.minigame.reset", 1);
getPermissionManager().registerPermission("Force Deathmatch build", "deathmatch.build.change", 1);

exec("./environmentLoader.cs");


function DM_getBrickGroup() {
	return BrickGroup_888888;
}

function DM_discoverBuilds() {
	$Deathmatch::Temp::BuildCount = 0;
	deleteVariables("$Deathmatch::Temp::BuildByName*");

	%fo = new FileObject();

	setModPaths(getModPaths()); // Refresh the file cache
	if (!isFile("config/server/deathmatch/builds/README.txt")) {
		%regex = "Add-Ons/GameMode_Deathmatch/defaults/builds/*.*";
		%targetFO = new FileObject();
		for (%i = findFirstFile(%regex) ; %i !$= "" ; %i = findNextFile(%regex)) {
			%targetPath = "config/server/deathmatch/builds/" @ fileName(%i);

			%fo.openForRead(%i); // Hack because fileCopy doesn't seem to like zip files
			%targetFO.openForWrite(%targetPath);

			while (!%fo.isEOF())
				%targetFO.writeLine(%fo.readLine());

			%fo.close();
			%targetFO.close();
		}
		%targetFO.delete();
	}

	%regex = "config/server/deathmatch/builds/*.dm";
	for (%i = findFirstFile(%regex) ; %i !$= "" ; %i = findNextFile(%regex)) {
		%fo.openForRead(%i);

		%name = trim(strReplace(fileName(fileBase(%i)), "_", " "));

		%build = "";
		%playerDatablock = PlayerStandardArmor;
		%toolCount = 0;

		%success = true;

		%oldGameModeArg = $gameModeArg; // Temporarily disable add-on whitelist
		$gameModeArg = "";

		%brickDamage = true;
		%fallingDamage = true;

		%environment = "";

		while(!%fo.isEOF()) {
			%line = %fo.readLine();

			switch$(getWord(%line, 0)) {
				case "addon":
					%addOn = getWords(%line, 1);
					$AddOn__[%addOn] = true; // Enable the add-on
					if (forceRequiredAddOn(%addOn) == $Error::AddOn_NotFound) {
						error("Add-On" SPC %addOn SPC "(required by" SPC %name @ ") was not found), skipping build" SPC %name);
						%success = false;
						break;
					}

				case "build":
					%build = getWords(%line, 1);

					if (!isFile(%build)) {
						error("Build file" SPC %build SPC "does not exist, skipping build" SPC %name);
						%success = false;
						break;
					}

				case "tool":
					%toolNum = getWord(%line, 1);
					%currTool = getWords(%line, 2);

					if (%toolNum !$= %toolNum + 0) {
						error(%toolNum SPC "is not a valid int, skipping build" SPC %name);
						%success = false;
						break;
					}

					if (!isObject(%currTool)) {
						error("Tool" SPC %currTool SPC "does not exist, skipping build" SPC %name);
						%success = false;
						break;
					}

					// TODO: Add more foolproofing

					%tools[%toolNum] = %currTool;
					%toolCount = getMax(%toolCount, %toolNum + 1);

				case "playertype":
					%playerDatablock = getWords(%line, 1);

					if (!isObject(%playerDatablock)) {
						error("Player type" SPC %playerDatablock SPC "does not exist, skipping build" SPC %name);
						%success = false;
						break;
					}

				case "brickdamage":
					%brickDamage = getWord(%line, 1) + 0;

				case "fallingdamage":
					%fallingDamage = getWord(%line, 1) + 0;

				case "environment":
					%environment = getWords(%line, 1);

					if (!isFile(%environment)) {
						error("Environment" SPC %environment SPC "does not exist, skipping build" SPC %name);
						%success = false;
						break;
					}
			}
		}

		if (%success && %build $= "") {
			error("Build manifest" SPC %name SPC "has no build file, skipping");
			%success = false;
		}

		if (%success) {
			%buildID = $Deathmatch::Temp::BuildCount;
			$Deathmatch::Temp::BuildCount++;

			$Deathmatch::Temp::BuildName[%buildID] = %name;
			$Deathmatch::Temp::BuildFile[%buildID] = %build;
			$Deathmatch::Temp::BuildPlayerDatablock[%buildID] = %playerDatablock;

			$Deathmatch::Temp::BuildBrickDamage[%buildID] = %brickDamage;
			$Deathmatch::Temp::BuildFallingDamage[%buildID] = %fallingDamage;

			$Deathmatch::Temp::BuildEnvironment[%buildID] = %environment;

			for (%i = 0 ; %i < %toolCount ; %i++)
				$Deathmatch::Temp::BuildTool[%buildID, %i] = %tools[%i];

			$Deathmatch::Temp::BuildByName[%name] = %buildID;

			echo("Added build" SPC %name SPC "with the ID" SPC %buildID);
		}

		$gameModeArg = %oldGameModeArg;

		%fo.close();
	}

	%fo.delete();
}

function DM_loadBuild(%build) {
	DM_getBrickGroup().deleteAll();

	%buildID = $Deathmatch::Temp::BuildByName[%build];

	if (%buildID $= "") {
		%msg = "DM ERROR: Build" SPC %build SPC "does not exist.";
		error(%msg);
		announce(%msg);
		return;
	}

	loadEnvironmentFromFile("Add-Ons/GameMode_Deathmatch/environment.txt");

	%env = $Deathmatch::Temp::BuildEnvironment[%buildID];
	if (%env !$= "")
		loadEnvironmentFromFile(%env);

	$DefaultMinigame.brickDamage = $Deathmatch::Temp::BuildBrickDamage[%buildID];
	$DefaultMinigame.fallingDamage = $Deathmatch::Temp::BuildFallingDamage[%buildID];

	$DefaultMinigame.playerDatablock = $Deathmatch::Temp::BuildPlayerDatablock[%buildID].getID();
	serverDirectSaveFileLoad($Deathmatch::Temp::BuildFile[%buildID], 3, "", 2);

	for (%i = 0 ; %i < $DefaultMinigame.playerDatablock.maxTools ; %i++) {
		%buildTool = $Deathmatch::Temp::BuildTool[%buildID, %i];
		if (isObject(%buildTool))
			$DefaultMinigame.startEquip[%i] = %buildTool.getID();
		else
			$DefaultMinigame.startEquip[%i] = 0;
	}

	$DefaultMinigame.build = %build;
}

function DM_getRandomBuilds(%count, %excludeCurrent) {
	%count = mClamp(%count, 1, $Deathmatch::Temp::BuildCount);

	%buildAdded[$DefaultMinigame.build] = true;

	%out = "";

	for (%i = 0 ; %i < %count ; %i++) {
		%buildID = getRandom(0, $Deathmatch::Temp::BuildCount - 1);
		%build = $Deathmatch::Temp::BuildName[%buildID];

		if (%buildAdded[%build]) {
			continue;
		}

		%buildAdded[%build] = true;
		if (%out $= "")
			%out = %build;
		else
			%out = %out TAB %build;
	}

	return %out;
}

function serverCmdVote(%cl, %a, %b, %c, %d, %e, %f, %g, %h) {
	%build = trim(%a SPC %b SPC %c SPC %d SPC %e SPC %f SPC %g SPC %h);

	if (%cl.miniGame.getID() != $DefaultMinigame.getID() || %cl.miniGame.buildVoteSchedule == 0)
		return;

	%buildID = $Deathmatch::Temp::BuildByName[%build];
	%build = $Deathmatch::Temp::BuildName[%buildID];

	if (%buildID $= "") {
		messageClient(%cl, '', "That build does not exist.");
		return;
	}

	if (!$Deathmatch::Temp::Vote::Eligible[%build]) {
		messageClient(%cl, '', "That build is not in the current vote.");
		return;
	}

	if ($Deathmatch::Temp::Vote::Voted[%cl.bl_id] !$= "") {
		messageClient(%cl, '', "You have already voted.");
		return;
	}

	announce("\c6" @ %cl.name SPC "\c3voted for\c6" SPC %build @ "\c3.");
	$Deathmatch::Temp::Vote::Voted[%cl.bl_id] = %build;
	$Deathmatch::Temp::Vote::MaxBL_ID = getMax($Deathmatch::Temp::Vote::MaxBL_ID, %cl.bl_id);
}

function MiniGameSO::startBuildVote(%this) {
	cancel(%this.scoreLimitSchedule);
	cancel(%this.timeLimitSchedule);
	if (%this.buildVoteSchedule)
		return;

	deleteVariables("$Deathmatch::Temp::Vote*");
	$Deathmatch::Temp::Vote::MaxBL_ID = 0;

	%eligible = DM_getRandomBuilds($Deathmatch::Pref::VoteBuildAmount, true);
	%eligible = trim(%eligible TAB %this.build);
	echo(%eligible);
	%eligibleCount = getFieldCount(%eligible);

	if (%eligibleCount == 1) {
		%this.nextBuild = %eligible;
		%this.reset(0);
	} else {
		%this.messageAll('', "\c3You may now vote for new builds. You have\c6" SPC $Deathmatch::Pref::VoteTimeLimit SPC "\c3seconds to vote. Your choices are:");

		for (%i = 0 ; %i < %eligibleCount ; %i++) {
			%build = getField(%eligible, %i);
			%this.messageAll('', "\c3*" @ (%this.build $= %build ? " Extend\c6" : "\c6") SPC %build);
			$Deathmatch::Temp::Vote::Eligible[%build] = true;
		}

		%this.messageAll('', "\c3You may vote by typing \"/vote \c7map\c3\" (without the quotes)");

		%this.buildVoteSchedule = %this.schedule($Deathmatch::Pref::VoteTimeLimit * 1000, endBuildVote);
	}
}

function MiniGameSO::endBuildVote(%this, %noReset) {
	cancel(%this.buildVoteSchedule);
	%this.buildVoteSchedule = 0;

	for (%i = 0 ; %i <= $Deathmatch::Temp::Vote::MaxBL_ID ; %i++) {
		%vote = $Deathmatch::Temp::Vote::Voted[%i];

		if (%vote !$= "") {
			%tally[%vote] += 1;
		}
	}

	%max = %this.build;

	for (%i = 0 ; %i < $Deathmatch::Temp::BuildCount ; %i++) {
		%name = $Deathmatch::Temp::BuildName[%i];
		if (%tally[%name] > %tally[%max]) {
			%max = %name;
		}
	}

	%this.nextBuild = %max;

	%this.messageAll('', "\c3The build vote is now over.\c6" SPC %this.nextBuild SPC "\c3won!");

	if (!%noReset)
		%this.reset(0);
}

function MiniGameSO::checkScoreLimit(%this) {
	if (%this.buildVoteSchedule)
		return;

	for (%i = 0 ; %i < ClientGroup.getCount() ; %i++) {
		%obj = ClientGroup.getObject(%i);
		if (!isObject(%obj.miniGame) || %obj.miniGame.getID() != %this.getID())
			continue;

		if (%obj.score >= $Deathmatch::Pref::ScoreLimit) {
			%this.startBuildVote();
			return;
		}
	}

	%this.scoreLimitSchedule = %this.schedule(1000, checkScoreLimit);
}


DM_discoverBuilds();


package DM {
	function MiniGameSO::onAdd(%this) {
		%ret = parent::onAdd(%this);

		%this.schedule(1000, resetDM);

		return %ret;
	}

	function MiniGameSO::resetDM(%this) {
		if (%this.getID() == $DefaultMinigame.getID())
			%this.reset(0);
	}

	function MiniGameSO::reset(%this, %client) {
		if (%this.getID() != $DefaultMinigame.getID())
			return parent::reset(%this, %client);

		if (%this.buildVoteSchedule)
			%this.endBuildVote(true);
		cancel(%this.timeLimitSchedule);
		cancel(%this.scoreLimitSchedule);

		if (%this.nextBuild $= "")
			%this.nextBuild = DM_getRandomBuilds(1);
		DM_loadBuild(%this.nextBuild);
		%this.nextBuild = "";

		if ($Deathmatch::Pref::TimeLimit != -1)
			%this.timeLimitSchedule = %this.schedule($Deathmatch::Pref::TimeLimit * 1000, startBuildVote);

		if ($Deathmatch::Pref::ScoreLimit != -1)
			%this.scoreLimitSchedule = %this.schedule(1000, checkScoreLimit);

		return parent::reset(%this, %client);
	}

	function serverLoadSaveFile_End() {
		$DefaultMinigame.schedule(0, respawnAll);

		return parent::serverLoadSaveFile_End();
	}

	function serverCmdResetMinigame(%cl) {
		if (%cl.miniGame.getID() != $DefaultMinigame.getID())
			parent::serverCmdResetMinigame(%cl);
		else if (%cl.hasPermission("deathmatch.minigame.reset"))
			$DefaultMinigame.reset(0);
	}

	function serverCmdChangeBuild(%cl, %a, %b, %c, %d, %e, %f) {
		if (!%cl.hasPermission("deathmatch.build.change") || %cl.miniGame.getID() != $DefaultMinigame.getID())
			return;

		%build = trim(%a SPC %b SPC %c SPC %d SPC %e SPC %f);
		%buildID = $Deathmatch::Temp::BuildByName[%build];
		%build = $Deathmatch::Temp::BuildName[%buildID];

		if (%build $= "")
			return;

		$DefaultMinigame.nextBuild = %build;
		$DefaultMinigame.messageAll('', "\c6" @ %cl.name SPC "\c3changed the build to\c6" SPC %build @ "\c3.");
		$DefaultMinigame.reset(0);
	}
}; activatePackage(DM);
