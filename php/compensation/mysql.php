<?php
	static $SQL_LINK;
	static $Connected = false;

	function sqlLink()
	{
		global $SQL_LINK;
		return $SQL_LINK;
	}

	function SQLConnect()
	{
		/*$SQL_HOST = "mysql.hostinger.ru";
		$SQL_LOGIN = "u322760627_bosik";
		$SQL_PASS = "iAsaO7STz2ctsW9iPqbC";
		$SQL_DATABASE = "u322760627_db";*/

		$SQL_HOST  = "127.0.0.1:3306";
		$SQL_LOGIN = "root";
		$SQL_PASS = "root";
		$SQL_DATABASE = "compensation"; 

		global $Connected;

		if (!$Connected)
		{
			global $SQL_LINK;
			$SQL_LINK = mysqli_connect($SQL_HOST, $SQL_LOGIN, $SQL_PASS) or die(mysql_error());
			mysqli_select_db(sqlLink(), $SQL_DATABASE) or die(mysql_error());

			mysqli_query(sqlLink(), "set character_set_client	= 'cp1251'");
			mysqli_query(sqlLink(), "set character_set_results	= 'cp1251'");
			mysqli_query(sqlLink(), "set collation_connection	= 'cp1251_general_ci'");

			$Connected = true;
		}
	}
	SQLConnect();
?>