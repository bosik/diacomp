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
				"		<title>Компенсация Онлайн</title>\n" .
				"		<link type=\"text/css\" rel=\"stylesheet\" href=\"http://www.dreamtemplate.com/dreamcodes/flat_buttons/css/tsc_flat_buttons.css\" />\n" .
				"		<link type=\"text/css\" rel=\"stylesheet\" href=\"css/style.css\" />\n" .
				"	</head>\n" .
				"	<body>\n" .
				"		<div class=\"main\">\n" .
				"			<div class=\"header\">\n" .
				"				<a href=\"index.php\" title=\"На главную\"><div class=\"title\">\n" .
				"					<img class=\"icon\" src=\"img/icon.jpg\">Компенсация\n" .
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
							"				<a href=\"index.php\"><span class=\"menu_item " . c(1, $selected) . "\">О проекте</span></a>\n" .
							"				<a href=\"download.php\"><span class=\"menu_item " . c(2, $selected) . "\">Скачать</span></a>\n" .
							"			</div>";
					break;
				}
			case SCREEN_AUTH:
				{
					return
							"			<div class=\"menu\">\n" .
							"				<a href=\"login.php\"><span class=\"menu_item " . c(1, $selected) . "\">Авторизация</span></a>\n" .
							"				<a href=\"reg.php\"><span class=\"menu_item " . c(2, $selected) . "\">Регистрация</span></a>\n" .
							"			</div>";
					break;
				}
			case SCREEN_USER:
				{
					return
							"			<div class=\"menu\">\n" .
							"				<a href=\"index.php\"><span class=\"menu_item " . c(1, $selected) . "\">Дневник</span></a>\n" .
							"				<a href=\"#\" title=\"В разработке\"><span class=\"menu_item " . c(2, $selected) . "\">Базы</span></a>\n" .
							"				<a href=\"#\" title=\"В разработке\"><span class=\"menu_item " . c(3, $selected) . "\">Статистика</span></a>\n" .
							"				<a href=\"download.php\"><span class=\"menu_item " . c(4, $selected) . "\">Скачать</span></a>\n" .
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
				"				<a href=\"login.php?logout\">Выход</a>\n" .
				"			</div>";
	}

	function BlockUserAnon()
	{
		return
				"			<div class=\"login hint\">\n" .
				"				<a href=\"login.php\">Войти</a>\n" .
				"			</div>";
	}

	function BlockAppInfo()
	{
		return
				"			<div class=\"welcome_info\">\n" .
				"				<b>Что такое Компенсация?</b><br/>\n" .
				"				Компенсация — это бесплатный электронный дневник, предназначенный для больных сахарным диабетом I типа.<br/>\n" .
				"				<br/>\n" .
				"				<b>Для чего она нужна?</b><br/>\n" .
				"				Программа позволяет вести дневник, записывая в него данные о значениях сахара крови (СК), дозах инсулина и всей съеденной пище. " .
				"Главные отличия Компенсации от других программ — возможность комплексного анализа полученной информации; автоматическое вычисление " .
				"и адаптация коэффициентов, расчёт необходимой дозы инсулина; балансирование рациона.<br/>\n" .
				"				<br/>\n" .
				"				<b>С чего начать?</b><br/>\n" .
				"				Скачать дневник можно <b><a href=\"download.php\">здесь</a></b>.\n" .
				"			</div>";
	}

	function BlockDateForm($date, $page)
	{
		return
				"			<div class=\"date_panel\" action=\"index.php\" method=\"get\">\n" .
				"				Дата\n" .
				"				<input type=\"date\" id=\"calendar\"name=\"date\" size=\"14\" value=\"" . $date . "\" onchange=\"onDateChanged(this)\"/>\n" .
				"				<input type=\"hidden\" id=\"origin_date\" value=\"" . $date . "\"/>\n" .
				"				<!--button onclick=\"openPage()\" class=\"tsc_awb_small tsc_awb_white tsc_flat\">Перейти</button-->\n" .
				"				<button onclick=\"prevDay()\" class=\"tsc_awb_small tsc_awb_white tsc_flat\" title=\"Предыдущий день\">&lt;</button>\n" .
				"				<button onclick=\"nextDay()\" class=\"tsc_awb_small tsc_awb_white tsc_flat\" title=\"Следующий день\">&gt;</button>\n" .
				"				<div class=\"hint\" style=\"float: right; text-align: right;\">\n" . 
				"					<div id=\"modspan\" title=\"Последнее изменение\"></div>\n" .
				"					<div id=\"versionspan\" title=\"Версия\"></div>\n" .
				"				</div>\n" .
				
				"			</div>\n" .
				$page;
	}

	function BlockDownloads()
	{
		return
				"			<div style=\"padding-top: 5px\">\n" .
				"				<a href=\"app/Compensation_1.08.zip\" class=\"button_download windows\">Компенсация (Windows)</a>\n" .
				"				<a href=\"#\" class=\"button_download android\" onclick=\"soon()\">Компенсация (Android)</a>\n" .
				"			</div>\n" .
				"                       <link type=\"text/css\" rel=\"stylesheet\" href=\"css/download.css\" />\n" .
				"			<script type=\"text/javascript\">\n" .
				"				function soon()\n" .
				"				{\n" .
				"					alert(\"Приложение для Android находится в разработке\");\n" .
				"				}\n" .
				"			</script>";
	}

	function BlockLoginForm($redir, $with_error)
	{
		$result =
				"			<div class=\"login_panel\">\n" .
				"				<form action=\"login.php\" method=\"post\">\n" .
				"					Email:<br/><input type=\"text\" name=\"login\" class=\"full_width\"/><br/>\n" .
				"					Пароль:<br/><input type=\"password\" name=\"password\" class=\"full_width\"/><br/>\n" .
				"					<input type=\"submit\" class=\"full_width\" name=\"log\" value=\"Вход\" />\n" .
				"					<input type=\"hidden\" name=\"redir\" value=\"" . $redir . "\" />\n" .
				"				</form>\n";

		if ($with_error)
			$result .=
					"				<div class=\"error_login\">Неверный логин или пароль</div>\n";

		$result .=
				"				<hr>Нет учётной записи? <a href=\"reg.php\">Зарегистрируйтесь</a>.\n" .
				"			</div>\n";

		/*
		  '<form action="login.php" method="post">
		  <small>Email:</small><br/><input type="text" name="login"  size="25"/><br/>
		  <small>Пароль:</small><br/><input type="password" name="password"  size="25"/><br/>
		  <p><input type="submit" name="log" value="Вход" />
		  </form>
		  <a href="reg.php">Зарегистрироваться</a>
		  ';

		  if ($with_error)
		  $result .= '<br/><br/> <h2> Неверный логин или пароль </h2>'; */

		return $result;
	}

	function BlockRegForm($login, $password, $with_error)
	{
		$result =
				"			<div class=\"login_panel\">\n" .
				"				<form action=\"reg.php\" method=\"post\">\n" .
				"					Email:<br/>\n" .
				"					<input type=\"text\" name=\"login\" class=\"full_width\" value=\"" . $login . "\"/><br/>\n" .
				"					Пароль:<br/>\n" .
				"					<input type=\"password\" name=\"password\" class=\"full_width\" value=\"" . $password . "\"/><br/>\n" .
				"					<img src=\"kcaptcha/?" . session_name() . "=" . session_id() . "\" class=\"full_width\" style=\"padding: 10px 0px 10px;\"><br/>\n" .
				"					Код с картинки:<br/>\n" .
				"					<input type=\"text\" name=\"captcha\" class=\"full_width\"/><br/>\n" .
				"					<input type=\"submit\" class=\"full_width\" name=\"reg\" value=\"Регистрация\" />\n" .
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
				"				<div class=\"reg_success\">Для завершения регистрации перейдите по ссылке в письме, которое было отправлено на ваш адрес «" . $to . "»</div>" .
				"			</div>\n";
	}

	function PrintActivationSucceed()
	{
		echo 'Регистрация завершена.';
	}

	/**
	 * Возвращает указанную страницу в виде HTML-кода
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
			return $tmp . " ед";
		}

		function GetBloodValue($s)
		{
			$names = array(
				"-1" => "",
				"0" => "БЛ",
				"1" => "1Л",
				"2" => "2Л",
				"3" => "3Л",
				"4" => "4Л",
				"5" => "4П",
				"6" => "3П",
				"7" => "2П",
				"8" => "1П",
				"9" => "БП"
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
					"				Дневник временно доступен только для просмотра. Для добавления новых записей и редактирования <a href=\"download.php\">установите приложение</a>.\n" .
					"			</div>";
		}*/
		$result = "";

		/*
		  if ($text == "")
		  {
		  $result .=
		  "			<div class=\"empty_page\">\n" .
		  "				Страница пуста\n" .
		  "			</div>\n";
		  }
		  else
		  {
		  $prev_food = false;
		  $result .= "			<div class=\"diary\">\n";

		  $lines = GetLines($text);

		  // TODO: убрать этот ужас
		  for ($i = 0; $i < count($lines); $i++)
		  {
		  // определяем тип записи
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
//		$result .= "				<input type=\"submit\" value=\"Добавить\">\n";
//		$result .= "			</form>\n";


//		$result .= Footer();

		return $result;
	}
?>
