<?php
	require_once 'safe_slash.php'; // ���������� ��������� ����������
	require_once 'process.php'; // GetPage()
	require_once 'common.php'; // <--- "session_start();" inside
	require_once 'printer.php';
	require_once 'mysql.php'; // to establish connection

	$userinfo = "";
	$menu = "";
	$content = "";
	$javascript = "";

	if (IsLogged())
	{
		$cur_id = GetUserId();
		$cur_name = GetUserName();
		$userinfo = BlockUserLogged($cur_name);

		$date = false;

		// TODO: ����� �� ��������� ���?
		if (!empty($_GET))
		{
			if (isset($_GET['date']))
			{
				$date = mysqli_real_escape_string(sqlLink(), $_GET['date']);
				if (Valid($date))
				{
					$_SESSION[S_CURDATE] = $date;
				}
			}
		}

		// ���� ��� � �������� ��������������������
		if (!$date)
		{
			// ������ ������ - ���������� ���������� ����
			if (!isset($_SESSION[S_CURDATE]))
				$_SESSION[S_CURDATE] = gmdate(STD_DATE_FMT); // ������� ����
			$date = $_SESSION[S_CURDATE];
		}

		$page = GetPage($cur_id, $date, false);
		// $json = GetPageJSON($cur_id, $date);
		$menu = BlockMenu(SCREEN_USER, 1);
		$content = BlockDateForm($date, BlockPageHTML($page));
		$javascript = "		<script type=\"text/javascript\" src=\"js/diary.js\"></script>";

		/* $javascript .= "\n		<script type=\"text/javascript\">\n" .
		  "			var page = " . $json . ";\n".
		  "</script>"; */
	} else
	{
		if (isset($_GET['date']))
		{
			$date = mysqli_real_escape_string(sqlLink(), $_GET['date']);
			header('Location: login.php?redir=index.php?date=' . $date);
		}
		else
		{
			$userinfo = BlockUserAnon();
			$menu = BlockMenu(SCREEN_WELCOME, 1);
			$content = BlockAppInfo();
		}
	}

	echo BlockMain($userinfo, $menu, $content, $javascript);
?>