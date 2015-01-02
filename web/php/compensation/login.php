<?php
	require_once 'safe_slash.php';
	require_once 'common.php'; // <--- "session_start();" inside
	require_once 'printer.php';

	// если требуется выйти
	if (isset($_GET['logout']))
	{
		Logout();
		// и переносим его на главную
		if (!isset($_GET['noredir']))
			header('Location: index.php');
		exit;
	}

	// получение статуса авторизации
	if (isset($_GET['status']))
	{
		if (IsLogged())
			echo 'online';
		else
			echo 'offline';
		exit;
	}

	// если это первый запрос
	if (empty($_POST))
	{
		if (IsLogged())
		{
			header('Location: index.php');
		}
		else
		{
			if (isset($_GET['redir']))
				$redir = mysqli_real_escape_string(sqlLink(), $_GET['redir']);
			else
				$redir = "";

			$userinfo = "";
			$menu = BlockMenu(SCREEN_AUTH, 1);
			$content = BlockLoginForm($redir, false);
			echo BlockMain($userinfo, $menu, $content);
		}

		exit;
	}

	// если это второй запрос
	// !empty($_POST)
	{
		//echo "<!--POST NOT FIRST -->\n";
		if ((isset($_POST['login'])) && (isset($_POST['password'])))
		{
			//echo "<!--login/password received -->\n";
			// авторизация
			$login = $_POST['login'];
			$password = $_POST['password'];
			$noredir = isset($_POST['noredir']);
			$checkAPI = isset($_POST['api']);

			if ($checkAPI && ($_POST['api'] != CURRENT_API_VERSION))
			{
				echo RESULT_FAIL . "|" . DESC_DEPAPI;
				//echo "<!-- api version failed -->\n";
			}
			else

			if (Login($login, $password))
			{
				if ($noredir)
					echo RESULT_DONE . "|" . gmdate(STD_TIME_FMT);
				else
				{
					if ($_POST['redir'] != "")
					{
						$redir = mysqli_real_escape_string(sqlLink(), $_POST['redir']);
						header('Location: ' . $redir);
					}
					else
					{
						header('Location: index.php');
					}
				}
				//echo "<!-- logged ok -->\n";
			}
			else
			{
				if ($noredir)
					echo RESULT_FAIL . "|" . DESC_BADNAME;
				else
				{
					if (isset($_POST['redir']))
						$redir = mysqli_real_escape_string(sqlLink(), $_POST['redir']);
					else
						$redir = "";

					$userinfo = "";
					$menu = BlockMenu(SCREEN_AUTH, 1);
					$content = BlockLoginForm($redir, true);
					echo BlockMain($userinfo, $menu, $content);
				}
				//echo "<!-- failed to login -->\n";
			}
		}
		else
		{
			//echo "<!-- unrecognized POST -->\n";
			// какой-то левый POST
			if ($noredir)
				echo RESULT_FAIL;
			else
			{
				$userinfo = "";
				$menu = BlockMenu(SCREEN_AUTH, 1);
				$content = BlockLoginForm("", false);
				echo BlockMain($userinfo, $menu, $content);
			}
		}
	}
?>