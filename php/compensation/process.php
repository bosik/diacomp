<?php
	// работа с SQL
	include_once 'mysql.php';
	include_once 'DiaryPage.php';
	include_once 'common.php';

	class Response
	{
		public $status;
		public $message;

		function __construct($status, $message)
		{
			$this->status = $status;
			$this->message = $message;
		}

		public function SaveToJSON()
		{
			//$this->Encode('windows-1251', 'utf-8');
			$result = json_encode($this);
			//$this->Encode('utf-8', 'windows-1251');

			return $result;
		}
	}

	/**
	 * Получает страницу с сервера
	 * @param type $user_id Идентификатор пользователя
	 * @param type $date Дата
	 * @return string Cтраница (может быть пустой, если она не найдена
	 * в базе или возникли ошибки)
	 */
	/* function GetPage($user_id, $date)
	  {
	  $query = "SELECT D.Page FROM `Diary` D WHERE D.UserID='{$user_id}' AND D.Date='{$date}' LIMIT 1";
	  $sql = mysqli_query(sqlLink(), $query);
	  if ($sql == false)
	  return ''; // !!! или false?

	  if (mysqli_num_rows($sql) == 1)
	  {
	  $row = mysqli_fetch_assoc($sql);
	  return $row['Page'];
	  }
	  else
	  return '';
	  } */

	/**
	 * Получает страницу с сервера вместе с заголовком, содержащим дату, timestamp и версию
	 * @param type $user_id Идентификатор пользователя
	 * @param type $date Дата загружаемой страницы
	 * @return string Cтраница
	 */
	/* function GetPageHeaded($user_id, $date)
	  {
	  $query = "SELECT D.Page, D.TimeStamp, D.Version FROM `Diary` D WHERE D.UserID='{$user_id}' AND D.Date='{$date}' LIMIT 1";
	  $sql = mysqli_query(sqlLink(), $query);
	  if ($sql == false)
	  return ''; // !!! или false?

	  if (mysqli_num_rows($sql) == 1)
	  {
	  $row = mysqli_fetch_assoc($sql);
	  return "=== " . $date . " ===|" . $row['TimeStamp'] . "|" . $row['Version'] . "\n" . $row['Page'];
	  }
	  else
	  return "=== " . $date . " ===|" . date(STD_TIME_FMT) . "|0" . "\n"; // не найдена
	  } */

	// TODO: оптимизировать, запрашивая сразу несколько страниц

	function GetPage($user_id, $date, $headed)
	{
		$query = "SELECT D.Page, D.TimeStamp, D.Version FROM `Diary` D WHERE D.UserID='{$user_id}' AND D.Date='{$date}' LIMIT 1";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return ''; // !!! или false?

		if (mysqli_num_rows($sql) == 1)
		{
			// существует, достаём из базы
			$row = mysqli_fetch_assoc($sql);
			if ($headed)
				return "=== " . $date . " ===|" . $row['TimeStamp'] . "|" . $row['Version'] . "\n" . $row['Page'];
			else
				return $row['Page'];
		}
		else
		{
			// создаём новую
			if ($headed)
				return "=== " . $date . " ===|" . date(STD_TIME_FMT, strtotime('1970-01-01')) . "|0" . "\n"; // не найдена
			else
				return "";
		}
	}

	/**
	 * Выгружает страницу на сервер. <i>Пустые страницы не добавляются.</i>
	 * @param int $user_id Идентификатор пользователя
	 * @param string $date Дата
	 * @param string $page_source Страница
	 * @return bool true, если выгрузка прошла успешно, false в противном случае
	 */
	function PostPage($user_id, $date, $page_source, $stamp, $version)
	{
		// проверяем, есть ли такая запись в базе
		$query = "SELECT `PageID` FROM `Diary` D WHERE D.UserID='{$user_id}' AND D.Date='{$date}' LIMIT 1";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
		{
			return new Response(1, "Can't upload page: date='" . $date . "' timestamp='" . $stamp . "' version='" . $version . "': " . mysqli_error(sqlLink()));
		}

		$page_source = mysqli_real_escape_string(sqlLink(), $page_source); // можно ли?
		// если есть — обновляем
		if (mysqli_num_rows($sql) == 1)
		{
			$row = mysqli_fetch_assoc($sql);
			$pageid = $row['PageID'];
			$query = "UPDATE `Diary` SET `Page`='{$page_source}', `TimeStamp`='{$stamp}', `Version`='{$version}' WHERE PageID='{$pageid}'";

			$sql = mysqli_query(sqlLink(), $query); //true if succeed, false otherwise
			if ($sql == false)
			{
				return new Response(1, "Can't upload page: date='" . $date . "' timestamp='" . $stamp . "' version='" . $version . "': " . mysqli_error(sqlLink()));
			}
			else
			{
				return new Response(0, "Page posted OK");
			}
		}
		else
		// если нет - добавляем
		{
			//if ($page_source != "")
			if (true)
			{
				$query = "INSERT INTO `Diary` (`UserID`, `Date`, `TimeStamp`, `Version`, `Page`) " .
						"VALUES ('{$user_id}', '{$date}', '{$stamp}', '{$version}', '{$page_source}');";

				$sql = mysqli_query(sqlLink(), $query); // true if succeed, false otherwise

				if ($sql == false)
				{
					return new Response(1, "Can't upload page: date='" . $date . "' timestamp='" . $stamp . "' version='" . $version . "': " . mysqli_error(sqlLink()));
				}
				else
				{
					return new Response(0, "Page posted OK");
				}
			}
//			else
//			{
//				// тихо игнорируем пустые страницы
//				return true;

			/* Почему нельзя игнорировать добавление пустых страниц:
			 *
			 * CLIENT_1: {page("xxx", v5)}
			 * CLIENT_2: {page("xxx", v5)}
			 * SERVER:   {}
			 *
			 * ...CLIENT_1 empties page...
			 *
			 * CLIENT_1: {page("", v6)}
			 * CLIENT_2: {page("xxx", v5)}
			 * SERVER:   {}
			 *
			 * ...Sync(CLIENT_1, SERVER)...
			 *
			 * CLIENT_1: {page("", v6)}
			 * CLIENT_2: {page("xxx", v5)}
			 * SERVER:   {}
			 *
			 * - В этой ситуации два клиента синхронизированы без посредства сервера - так не бывает
			 * - БЫВАЕТ! Например, просто скопировали программу на флешке.
			 */
//			}
		}
	}

	function GetPageJSON($user_id, $date)
	{
		$page_source = GetPage($user_id, $date, true);
		$page = new DiaryPage();
		$page->LoadFromPlain($page_source);
		return $page->SaveToJSON();
	}

	function PostPageJSON($user_id, $json)
	{
		//$json = mb_convert_encoding($json, 'utf-8', 'windows-1251');
		//$raw = new DiaryPage();
		//$raw = json_decode($json);
		//$json = str_replace('"', '\\"', $json);
		//echo "PostPageJSON; ID='", $user_id, "'\nJSON=<", $json, ">\n\n";

		$page = new DiaryPage();

		Cast(json_decode($json), $page);

		$page->Encode('utf-8', 'windows-1251');
		$page_source = $page->SaveToPlain(false);
		$date = $page->date;

		// TODO: debug
		/* echo
		  "*** FAKE POSTING ***\n",
		  "User ID: ", $user_id, "\n",
		  "Date: ", $date, "\n",
		  "Timestamp: ", $page->timestamp, "\n",
		  "Version: ", $page->version, "\n",
		  $page_source;
		  return true; */

		return PostPage($user_id, $date, $page_source, $page->timestamp, $page->version);
	}

	/**
	 * Получает информацию о дате/времени последней модификации страницы
	 * @param int $user_id Идентификатор пользователя
	 * @param type $date Дата страницы
	 * @return string Дата/время последней модификации страницы, если таковая найдена;
	 * 0, если страница не существует,
	 * $RESULT_FAIL, если возникли ошибки
	 */
	function GetPageTimeStamp($user_id, $date)
	{
		global $RESULT_FAIL;
		$query = "SELECT D.TimeStamp FROM `Diary` D WHERE D.UserID='{$user_id}' AND D.Date='{$date}' " . "LIMIT 1";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return $RESULT_FAIL;

		if (mysqli_num_rows($sql) == 1)
		{
			$row = mysqli_fetch_assoc($sql);
			return $row['TimeStamp'];
		}
		else
			return "0";
	}

	function GetModList($user_id, $time)
	{
		require_once 'mysql.php';
		$query = "SELECT `Date`, `Version` FROM `Diary` WHERE `UserID`='{$user_id}' AND `TimeStamp` > '{$time}'";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return ''; // !!! или false?

		$result = "";

		while ($row = mysqli_fetch_assoc($sql/* , MYSQL_NUM */))
		{
			$result .= $row['Date'] . "|" . $row['Version'] . "\n";
		}

		return $result;
	}

	function GetModListForDates($user_id, $dates)
	{
		require_once 'mysql.php';

		//$dates_set = "";

		$result = "";

		for ($i = 0; $i < count($dates); $i++)
		{
			//$dates_set .= "'" . $dates{$i} . "'";
			//if ($i < count($dates) - 1)
			//	$dates_set .= ", ";
			$query = "SELECT `Date`, `Version` FROM `Diary` WHERE `UserID`='{$user_id}' AND `Date`='{$dates{$i}}'";
			$sql = mysqli_query(sqlLink(), $query);
			if ($sql == false)
				return mysqli_error(sqlLink()); // !!! или false?
			if (mysqli_num_rows($sql) == 1)
			{
				$row = mysqli_fetch_assoc($sql);
				$result .= $row['Date'] . "|" . $row['Version'] . "\n";
			}
			else
			{
				$result .= $dates{$i} . "|" . "0" . "\n";
			}
		}

		//$query = "SELECT `Date`, `Version` FROM `Diary` WHERE `UserID`='{$user_id}' AND `Date` in ({$dates_set})";

		return $result;
	}

	function UploadPages($user_id, $pages)
	{

		function Separate($s, &$before, $separator, &$after)
		{
			$k = strpos($s, $separator);
			if ($k == false)
			{
				$before = $s;
				$after = '';
			}
			else
			{
				$before = substr($s, 0, $k);
				$after = substr($s, $k + 1);
			}

			//echo "<Separate> '" . $s . "' was separated into '" . $before . "' AND '" . $after . "' </Separate>\n";
		}

		function ParseHeader($s, &$date, &$timestamp, &$version)
		{
			$date = substr($s, 4, 10);
			$s1 = "";
			$s2 = "";
			Separate($s, $s1, '|', $s2);

			/* if (s2 == '')
			  {
			  // нештатная ситуация
			  $timestamp = $date;
			  $version = 1;
			  }
			  else
			  {
			  $tmp = s2;
			  Separate($tmp, $s1, '|', $s2);
			  $timestamp = $s1;

			  if (s2 == '')
			  {
			  // нештатная ситуация
			  $version = 1;
			  }
			  else
			  $version = $s2;
			  } */

			$s = $s2;
			Separate($s, $timestamp, '|', $version);
		}
		// DONE: преобразовать в массив строк
		// DONE: написать парсер заголовка
		// DONE: просканировать массив, распарсить заголовок/данные, отправить в базу

		$lines = GetLines($pages);
		$date = false;
		$timestamp = "";
		$version = "";
		$page = "";

		for ($i = 0; $i < count($lines); $i++)
		{
			//echo "<Line #" . $i . ">" . $lines{$i} . "</Line>\n";

			if ($lines{$i}{0} == '=')
			{
				if ($date != false)
				{
					//echo "<PostPage (inner) date=" . $date . "; stamp=" . $timestamp . "; version=" . $version . "; page: " . $page . "</PostPage (inner)>\n";
					$response = PostPage($user_id, $date, $page, $timestamp, $version);
					if ($response->status != 0)
					{
						return $response;
					}
				}
				ParseHeader($lines{$i}, $date, $timestamp, $version);
				//echo "<ParseHeader date=" . $date . "; stamp=" . $timestamp . "; version=" . $version . "</ParseHeader>\n";
				$page = '';
			}
			else
			{
				$page .= $lines{$i} . "\n";
			}
		}

		//echo "<PostPage (2) (inner) date=" . $date . "; stamp=" . $timestamp . "; version=" . $version . "; page: " . $page . "</PostPage (inner)>\n";
		$response = PostPage($user_id, $date, $page, $timestamp, $version);
		if ($response->status != 0)
		{
			return $response;
		}

		return new Response(0, "Pages uploaded OK");
	}

	//=======================================================

	function GetBlobBaseVersion($table, $user_id)
	{
		$query = "SELECT `Version` FROM `{$table}` WHERE `UserID`='{$user_id}'";

		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return "0";

		$row = mysqli_fetch_assoc($sql);
		if ($row)
		{
			return $row['Version'];
		}
		else
		{
			return "0";
		}
	}

	function DownloadBlobBase($table, $user_id)
	{
		$query = "SELECT Data FROM `{$table}` WHERE `UserID`='{$user_id}'";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return "";

		$row = mysqli_fetch_assoc($sql);
		if ($row)
		{
			return $row['Data'];
		}
		else
		{
			return "";
		}
	}

	function UploadBlobBase($table, $user_id, $data, $version)
	{
		// проверяем, есть ли такая запись в базе
		$query = "SELECT `UserID` FROM `{$table}` WHERE UserID='{$user_id}' LIMIT 1";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
		{
			return false;
		}

		// если есть - обновляем
		if (mysqli_num_rows($sql) == 1)
		{
			$query = "UPDATE `{$table}` SET `Version`='{$version}', `Data`='{$data}' WHERE UserID='{$user_id}'";
			return mysqli_query(sqlLink(), $query);
		}
		else
		// если нет - добавляем
		{
			$query = "INSERT INTO `{$table}` (`UserID`, `Version`, `Data`) VALUES ('{$user_id}', '{$version}', '{$data}');";
			return mysqli_query(sqlLink(), $query);
		}
	}

	//=======================================================

	function GetFoodbaseVersion($user_id)
	{
		return GetBlobBaseVersion('FoodBase', $user_id);
	}

	function DownloadFoodbase($user_id)
	{
		return DownloadBlobBase('FoodBase', $user_id);
	}

	function UploadFoodbase($user_id, $data, $version)
	{
		return UploadBlobBase('FoodBase', $user_id, $data, $version);
	}

	function GetDishbaseVersion($user_id)
	{
		return GetBlobBaseVersion('DishBase', $user_id);
	}

	function DownloadDishbase($user_id)
	{
		return DownloadBlobBase('DishBase', $user_id);
	}

	function UploadDishbase($user_id, $data, $version)
	{
		return UploadBlobBase('DishBase', $user_id, $data, $version);
	}

	function DownloadKoofs($user_id)
	{
		return DownloadBlobBase('Koof', $user_id);
	}

	function UploadKoofs($user_id, $data)
	{
		// no "version" info presented here
		// проверяем, есть ли такая запись в базе
		$query = "SELECT `UserID` FROM `Koof` WHERE UserID='{$user_id}' LIMIT 1";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
		{
			return false;
		}

		// если есть - обновляем
		if (mysqli_num_rows($sql) == 1)
		{
			$query = "UPDATE `Koof` SET `Data`='{$data}' WHERE UserID='{$user_id}'";
			return mysqli_query(sqlLink(), $query);
		}
		else
		// если нет - добавляем
		{
			$query = "INSERT INTO `Koof` (`UserID`, `Data`) VALUES ('{$user_id}', '{$data}');";
			return mysqli_query(sqlLink(), $query);
			//echo mysqli_error(sqlLink()) . "\n";
		}
	}

	function Search($key)
	{
		$data = array(
			"абрикос",
			"активиа творожная (клубника)",
			"апельсин",
			"банан",
			"вишня",
			"груша",
			"йогурт \"био-баланс\" кефирный 1%",
			"колбаса \"премиум\" (дубки)",
			"конфеты \"алёнка\" (красный Октябрь)",
			"молоко \"вкуснотеево\", 3,2%",
			"яблоко",
			"хлеб черный \"Премиум\"",
			"сахар",
			"сыр \"король артур\"",
			"печенье \"юбилейное\" молочное");

		$result = "";
		$key = "/" . $key . "/i";

		foreach ($data as $i => $value)
		{
			if (preg_match($key, $value))
			{
				$result .= $value . "\n";
			}
		}

		return $result;
	}
?>