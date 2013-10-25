<?php
	include_once 'process.php'; // GetLines()

	define("SCREEN_WELCOME", "1");
	define("SCREEN_AUTH", "2");
	define("SCREEN_USER", "3");

	function BlockMain($userinfo, $menu, $content, $javascript = "")
	{
		return
				"<!DOCTYPE html>\n" .
//				"<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.0//EN\" \"http://www.wapforum.org/DTD/xhtml-mobile10.dtd\">\n" .
				"<html>\n" .
				"	<head>\n" .
				"		<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">\n" .
				"		<title>����������� ������</title>\n" .
				"		<link type=\"text/css\" rel=\"stylesheet\" href=\"http://www.dreamtemplate.com/dreamcodes/flat_buttons/css/tsc_flat_buttons.css\" />\n" .
				"		<link type=\"text/css\" rel=\"stylesheet\" href=\"css/style.css\" />\n" .
				"	</head>\n" .
				"	<body>\n" .
				"		<div class=\"main\">\n" .
				"			<div class=\"header\">\n" .
				"				<a href=\"index.php\" title=\"�� �������\"><div class=\"title\">\n" .
				"					<img class=\"icon\" src=\"img/icon.jpg\">�����������\n" .
				"				</div></a>\n" .
				$userinfo . "\n" .
				"			</div>\n" .
				$menu . "\n" .
				$content . "\n" .
				"			<span id=\"debug\" class=\"hint\">"."</span>\n" . 
				"		</div>\n" .
				$javascript . "\n" .
				"	</body>\n" .
				"</html>";
	}

	function BlockMenu($type, $selected)
	{

		function c($index, $selected)
		{
			return ($index == $selected ? "menu_selected" : "menu_option");
		}
		switch ($type)
		{
			case SCREEN_WELCOME:
				{
					return
							"			<div class=\"menu\">\n" .
							"				<a href=\"index.php\"><span class=\"menu_item " . c(1, $selected) . "\">� �������</span></a>\n" .
							"				<a href=\"download.php\"><span class=\"menu_item " . c(2, $selected) . "\">�������</span></a>\n" .
							"			</div>";
					break;
				}
			case SCREEN_AUTH:
				{
					return
							"			<div class=\"menu\">\n" .
							"				<a href=\"login.php\"><span class=\"menu_item " . c(1, $selected) . "\">�����������</span></a>\n" .
							"				<a href=\"reg.php\"><span class=\"menu_item " . c(2, $selected) . "\">�����������</span></a>\n" .
							"			</div>";
					break;
				}
			case SCREEN_USER:
				{
					return
							"			<div class=\"menu\">\n" .
							"				<a href=\"index.php\"><span class=\"menu_item " . c(1, $selected) . "\">�������</span></a>\n" .
							"				<a href=\"#\" title=\"� ����������\"><span class=\"menu_item " . c(2, $selected) . "\">����</span></a>\n" .
							"				<a href=\"#\" title=\"� ����������\"><span class=\"menu_item " . c(3, $selected) . "\">����������</span></a>\n" .
							"				<a href=\"download.php\"><span class=\"menu_item " . c(4, $selected) . "\">�������</span></a>\n" .
							"			</div>";
					break;
				}
		}
	}

	function BlockUserLogged($username)
	{
		return
				"			<div class=\"login hint\">\n" .
				"				<div>" . $username . "</div>\n" .
				"				<a href=\"login.php?logout\">�����</a>\n" .
				"			</div>";
	}

	function BlockUserAnon()
	{
		return
				"			<div class=\"login hint\">\n" .
				"				<a href=\"login.php\">�����</a>\n" .
				"			</div>";
	}

	function BlockAppInfo()
	{
		return
				"			<div class=\"welcome_info\">\n" .
				"				<b>��� ����� �����������?</b><br/>\n" .
				"				����������� � ��� ���������� ����������� �������, ��������������� ��� ������� �������� �������� I ����.<br/>\n" .
				"				<br/>\n" .
				"				<b>��� ���� ��� �����?</b><br/>\n" .
				"				��������� ��������� ����� �������, ��������� � ���� ������ � ��������� ������ ����� (��), ����� �������� � ���� ��������� ����. " .
				"������� ������� ����������� �� ������ �������� � ����������� ������������ ������� ���������� ����������; �������������� ���������� " .
				"� ��������� �������������, ������ ����������� ���� ��������; �������������� �������.<br/>\n" .
				"				<br/>\n" .
				"				<b>� ���� ������?</b><br/>\n" .
				"				������� ������� ����� <b><a href=\"download.php\">�����</a></b>.\n" .
				"			</div>";
	}

	function BlockDateForm($date, $page)
	{
		return
				"			<div class=\"date_panel\" action=\"index.php\" method=\"get\">\n" .
				"				����\n" .
				"				<input type=\"date\" id=\"calendar\"name=\"date\" size=\"14\" value=\"" . $date . "\" onchange=\"onDateChanged(this)\"/>\n" .
				"				<input type=\"hidden\" id=\"origin_date\" value=\"" . $date . "\"/>\n" .
				"				<!--button onclick=\"openPage()\" class=\"tsc_awb_small tsc_awb_white tsc_flat\">�������</button-->\n" .
				"				<button onclick=\"prevDay()\" class=\"tsc_awb_small tsc_awb_white tsc_flat\" title=\"���������� ����\">&lt;</button>\n" .
				"				<button onclick=\"nextDay()\" class=\"tsc_awb_small tsc_awb_white tsc_flat\" title=\"��������� ����\">&gt;</button>\n" .
				"				<div class=\"hint\" style=\"float: right; text-align: right;\">\n" . 
				"					<div id=\"modspan\" title=\"��������� ���������\"></div>\n" .
				"					<div id=\"versionspan\" title=\"������\"></div>\n" .
				"				</div>\n" .
				
				"			</div>\n" .
				$page;
	}

	function BlockDownloads()
	{
		return
				"			<div style=\"padding-top: 5px\">\n" .
				"				<a href=\"app/Compensation_1.08.zip\" class=\"button_download windows\">����������� (Windows)</a>\n" .
				"				<a href=\"#\" class=\"button_download android\" onclick=\"soon()\">����������� (Android)</a>\n" .
				"			</div>\n" .
				"                       <link type=\"text/css\" rel=\"stylesheet\" href=\"css/download.css\" />\n" .
				"			<script type=\"text/javascript\">\n" .
				"				function soon()\n" .
				"				{\n" .
				"					alert(\"���������� ��� Android ��������� � ����������\");\n" .
				"				}\n" .
				"			</script>";
	}

	function BlockLoginForm($redir, $with_error)
	{
		$result =
				"			<div class=\"login_panel\">\n" .
				"				<form action=\"login.php\" method=\"post\">\n" .
				"					Email:<br/><input type=\"text\" name=\"login\" class=\"full_width\"/><br/>\n" .
				"					������:<br/><input type=\"password\" name=\"password\" class=\"full_width\"/><br/>\n" .
				"					<input type=\"submit\" class=\"full_width\" name=\"log\" value=\"����\" />\n" .
				"					<input type=\"hidden\" name=\"redir\" value=\"" . $redir . "\" />\n" .
				"				</form>\n";

		if ($with_error)
			$result .=
					"				<div class=\"error_login\">�������� ����� ��� ������</div>\n";

		$result .=
				"				<hr>��� ������� ������? <a href=\"reg.php\">�����������������</a>.\n" .
				"			</div>\n";

		/*
		  '<form action="login.php" method="post">
		  <small>Email:</small><br/><input type="text" name="login"  size="25"/><br/>
		  <small>������:</small><br/><input type="password" name="password"  size="25"/><br/>
		  <p><input type="submit" name="log" value="����" />
		  </form>
		  <a href="reg.php">������������������</a>
		  ';

		  if ($with_error)
		  $result .= '<br/><br/> <h2> �������� ����� ��� ������ </h2>'; */

		return $result;
	}

	function BlockRegForm($login, $password, $with_error)
	{
		$result =
				"			<div class=\"login_panel\">\n" .
				"				<form action=\"reg.php\" method=\"post\">\n" .
				"					Email:<br/>\n" .
				"					<input type=\"text\" name=\"login\" class=\"full_width\" value=\"" . $login . "\"/><br/>\n" .
				"					������:<br/>\n" .
				"					<input type=\"password\" name=\"password\" class=\"full_width\" value=\"" . $password . "\"/><br/>\n" .
				"					<img src=\"kcaptcha/?" . session_name() . "=" . session_id() . "\" class=\"full_width\" style=\"padding: 10px 0px 10px;\"><br/>\n" .
				"					��� � ��������:<br/>\n" .
				"					<input type=\"text\" name=\"captcha\" class=\"full_width\"/><br/>\n" .
				"					<input type=\"submit\" class=\"full_width\" name=\"reg\" value=\"�����������\" />\n" .
				"				</form>\n";

		if ($with_error != "")
			$result .=
					"				<div class=\"error_login\">" . $with_error . "</div>\n";

		$result .=
				"			</div>\n";

		return $result;
	}

	function BlockRegSucceed($to)
	{
		return
				"			<div class=\"login_panel\">\n" .
				"				<div class=\"reg_success\">��� ���������� ����������� ��������� �� ������ � ������, ������� ���� ���������� �� ��� ����� �" . $to . "�</div>" .
				"			</div>\n";
	}

	function PrintActivationSucceed()
	{
		echo '����������� ���������.';
	}

	/**
	 * ���������� ��������� �������� � ���� HTML-����
	 * @param type $text
	 * @return string
	 */
	function BlockPageHTML($text)
	{

		function GetTime($s)
		{
			return substr($s, 1, 5);
		}

		function GetValue($s)
		{
			return substr($s, 7);
		}

		function GetInsValue($s)
		{
			$tmp = substr($s, 7);
			//return ((strlen($tmp) == 1) ? $tmp . ",0" : $tmp);
			return $tmp . " ��";
		}

		function GetBloodValue($s)
		{
			$names = array(
				"-1" => "",
				"0" => "��",
				"1" => "1�",
				"2" => "2�",
				"3" => "3�",
				"4" => "4�",
				"5" => "4�",
				"6" => "3�",
				"7" => "2�",
				"8" => "1�",
				"9" => "��"
			);

			$s = substr($s, 7);
			$sep = strpos($s, "|");
			$val = "";
			$finger = "";
			if (!$sep)
			{
				$val = $s;
				$finger = "";
			}
			else
			{
				$val = substr($s, 0, $sep);
				$finger = substr($s, $sep + 1);
				$finger = $names[$finger];
			}
			$val = (strlen($val) == 1) ? $val . ",0" : $val;
			return $val . "  " . $finger;
		}

		function GetFood($s)
		{
			return
					substr($s, 1, strpos($s, '[') - 1) . " (" .
					substr($s, strpos($s, ']') + 2) . ")";
		}

		function CloseMeal()
		{
			return
					"					</div>\n" .
					"				</div>\n";
		}

		/*function Footer()
		{
			return
					"			<br/>\n" .
					"			<hr>\n" .
					"			<div class=\"hint\">\n" .
					"				������� �������� �������� ������ ��� ���������. ��� ���������� ����� ������� � �������������� <a href=\"download.php\">���������� ����������</a>.\n" .
					"			</div>";
		}*/
		$result = "";

		/*
		  if ($text == "")
		  {
		  $result .=
		  "			<div class=\"empty_page\">\n" .
		  "				�������� �����\n" .
		  "			</div>\n";
		  }
		  else
		  {
		  $prev_food = false;
		  $result .= "			<div class=\"diary\">\n";

		  $lines = GetLines($text);

		  // TODO: ������ ���� ����
		  for ($i = 0; $i < count($lines); $i++)
		  {
		  // ���������� ��� ������
		  switch ($lines{$i}{0})
		  {
		  case "*":
		  {
		  if ($prev_food)
		  {
		  $result .= CloseMeal();
		  $prev_food = false;
		  }
		  $result .= "				<div class=\"rec blood\">\n";
		  $result .= "					<div class=\"time\">" . GetTime($lines{$i}) . "</div>\n";
		  $result .= "					<div class=\"item\">" . GetBloodValue($lines{$i}) . "</div>\n";
		  $result .= "				</div>\n";
		  break;
		  }
		  case "-":
		  {
		  if ($prev_food)
		  {
		  $result .= CloseMeal();
		  $prev_food = false;
		  }
		  $result .= "				<div class=\"rec ins\">\n";
		  $result .= "					<div class=\"time\">" . GetTime($lines{$i}) . "</div>\n";
		  $result .= "					<div class=\"item\">" . GetInsValue($lines{$i}) . "</div>\n";
		  $result .= "				</div>\n";
		  break;
		  }
		  case " ":
		  {
		  if ($prev_food)
		  {
		  $result .= CloseMeal();
		  $prev_food = false;
		  }
		  $result .= "				<div class=\"rec meal\">\n";
		  $result .= "					<div class=\"time\">" . GetTime($lines{$i}) . "</div>\n";
		  $result .= "					<div class=\"item\">\n";
		  break;
		  }
		  case "#":
		  {
		  $result .= "						<div class=\"food\">" . GetFood($lines{$i}) . "</div><br/>\n";
		  $prev_food = true;
		  break;
		  }
		  case "%":
		  {
		  if ($prev_food)
		  {
		  $result .= CloseMeal();
		  $prev_food = false;
		  }
		  $result .= "				<div class=\"rec note\">\n";
		  $result .= "					<div class=\"time\">" . GetTime($lines{$i}) . "</div>\n";
		  $result .= "					<div class=\"item\">" . GetValue($lines{$i}) . "</div>\n";
		  $result .= "				</div>\n";
		  break;
		  }
		  }
		  }

		  if ($prev_food)
		  {
		  $result .= CloseMeal();
		  $prev_food = false;
		  }

		  $result .= "			</div>\n";
		  } */
		$result .= "			<div id=\"diary_block\">\n";
		$result .= "				<div id=\"diary_info_column\">\n";
		$result .= "					<div id=\"filler\" style=\"height: 0px\" onclick=\"showInfoBox(-1)\" ></div>\n";
		$result .= "					<div id=\"diary_info\"></div>\n";
		$result .= "				</div>\n";
		$result .= "				<div id=\"diary_page\" class=\"diary_page_empty\"></div>\n";
		$result .= "			</div>";


//		$result .= "			<form action=\"\" onsubmit=\"test()\">\n";
//		$result .= "				<input type=\"text\" id=\"note_editor\" size=\"50\"/>";
//		$result .= "				<input type=\"submit\" value=\"��������\">\n";
//		$result .= "			</form>\n";


//		$result .= Footer();

		return $result;
	}
?>
