<?php
	// авторизация
	session_start();
	require_once 'mysql.php';

	// константы
	// переменные сессии
	define("S_USERID", "user_id");
	define("S_USERNAME", "user_name");
	define("S_CURDATE", "last_date");

	// версия API сервера
	define("CURRENT_API_VERSION", "1.2");

	// результаты
	define("RESULT_DONE", "DONE");
	define("RESULT_FAIL", "FAIL");
	define("DESC_BADNAME", "BADNAME");
	define("DESC_DEPAPI", "DEPAPI");

	// форматы даты/времени
	define("STD_DATE_FMT", "Y-m-d");
	define("STD_TIME_FMT", "Y-m-d H:i:s");

	function Answer($bool_value)
	{
		if ($bool_value)
			return RESULT_DONE;
		else
			return RESULT_FAIL;
	}

	function Valid($date)
	{
		try
		{
			new DateTime($date);
			return true;
		} catch (Exception $e)
		{
			return false;
		}
	}

	function ShiftDate($date, $shift)
	{
		if (Valid($date))
		{
			$temp = new DateTime($date);
			$temp->modify($shift . ' days');
			return $temp->format(STD_DATE_FMT);
		}
		else
		{
			return $date;
		}
	}

	/**
	 * Авторизовывает пользователя. Поступающие данные экранируются.
	 * @param string $login Логин
	 * @param string $password Пароль
	 * @return bool true, если всё прошло успешно, и false в противном случае
	 */
	function Login($login, $password)
	{
		$login = mysqli_real_escape_string(sqlLink(), $login);
		$password = mysqli_real_escape_string(sqlLink(), $password);

		// входные данные корректны
		if (($login != '') && ($password != ''))
		{
			$hash = md5($password);
			$query = "SELECT `id` FROM `User` WHERE `Login`='{$login}' AND `HashPass`='{$hash}' LIMIT 1";
			$sql = mysqli_query(sqlLink(), $query); //die(/*mysql_error()*/"Error");
			if ($sql == false)
			{
				//echo "(1) ", mysql_error();
				return false;
			}

			if (mysqli_num_rows($sql) == 1)
			{
				$row = mysqli_fetch_assoc($sql);
				$id = $row['id'];

				// обновляем дату последнего визита
				$date = gmdate(STD_TIME_FMT);
				$query = "UPDATE `User` SET `DateLogin`='{$date}' WHERE `ID`='{$id}'";
				$sql = mysqli_query(sqlLink(), $query);
				if ($sql == false)
				{
					//echo "(2) ", $query;
					return false;
				}

				//ставим метку в сессии (ID пользователя)
				$_SESSION[S_USERID] = $id;
				$_SESSION[S_USERNAME] = $login;

				// успех!
				return true;
			}
			else
			{
				//echo "Fatal: multiple/no items founded";
				return false;
			}
		}
		// входные данные некорректны
		else
		{
			//echo "Login or password is empty";
			return false;
		}
	}

	/**
	 * Отключает пользователя, убирая метки его идентификатора из сессии
	 */
	function Logout()
	{
		if (isset($_SESSION[S_USERID]))
			unset($_SESSION[S_USERID]);
		//setcookie('login', '', 0, "/");
		//setcookie('password', '', 0, "/");
	}

	/**
	 * Проверяет,авторизован ли пользователь. Идентификатор пользователя ищется
	 * в текущей сессии, а затем проверяется наличие такого идентификатора в базе.
	 * @return bool true, если пользователь авторизован, false в противном случае
	 */
	function IsLogged()
	{
		// если указанный идентификатор записан в сессии
		if (isset($_SESSION[S_USERID]))
		{
			// дополнительно проверяем, что ID есть в таблице пользователей
			// TODO: высокая нагрузка! М.б. убрать вторую проверку?

			$query = "SELECT `Login` FROM `User` WHERE `ID`='{$_SESSION[S_USERID]}' LIMIT 1";
			$sql = mysqli_query(sqlLink(), $query);
			if ($sql == false)
				return false;
			else
				return (mysqli_num_rows($sql) == 1);
		}
		else
			return false;
	}

	/**
	 * Получает идентификатор текущего пользователя
	 * @return int Идентификатор пользователя, если он авторизовался, и false
	 * в противном случае
	 */
	function GetUserID()
	{
		if (isset($_SESSION[S_USERID]))
			return $_SESSION[S_USERID];
		else
			return false;
	}

	/**
	 * Получает имя (логин) текущего пользователя
	 * @return int Имя (логин) пользователя, если он авторизовался, и false
	 * в противном случае
	 */
	function GetUserName()
	{
		if (isset($_SESSION[S_USERNAME]))
			return $_SESSION[S_USERNAME];
		else
			return false;
	}

	/**
	 * Генерирует случайную строку из 32 символов
	 * @return string Сгенерированная строка
	 */
	function GenerateKey()
	{
		$key = '';
		$pattern = '1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
		$counter = strlen($pattern) - 1;
		for ($i = 0; $i < 32; $i++)
		{
			$key .= $pattern{rand(0, $counter)};
		}
		return $key;
	}

	function ExistsUser($login)
	{
		$query = "SELECT `ID` FROM `User` WHERE `Login`='{$login}'";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return false;
		if (mysqli_num_rows($sql) > 0)
			return true;

		$query = "SELECT `ID` FROM `Activate` WHERE `Login`='{$login}'";
		$sql = mysqli_query(sqlLink(), $query);
		if ($sql == false)
			return false;
		return (mysqli_num_rows($sql) > 0);
	}

	function AddActivate($login, $hashpass, $key)
	{
		/* 		
		  // проверяем наличие записи на активацию
		  $query = "SELECT `ID` FROM `Activate` WHERE `Login` = '{$login}'";
		  $sql = mysqli_query(sqlLink(), $query);
		  if ($sql == false)
		  return false;

		  if (mysqli_num_rows($sql) == 0)
		  {
		  $query = "INSERT INTO `Activate` (`Login`, `HashPass`, `Key`, `Date`) VALUES (" .
		  "'{$login}', '{$hashpass}', '{$key}', '" . date(STD_TIME_FMT) . "')";
		  }
		  else
		  {
		  $query = "UPDATE `Activate` SET " .
		  "`HashPass`={$hashpass}, `Key`={$key}, `Date`='" . date(STD_TIME_FMT) .
		  "' WHERE `Login`='{$login}'";
		  }
		  return mysqli_query(sqlLink(), $query);
		 */

		//$query = "INSERT INTO `Activate` (`Login`, `HashPass`, `Key`, `Date`) VALUES (" . "'{$login}', '{$hashpass}', '{$key}', '" . date(STD_TIME_FMT) . "')";
		$date = gmdate(STD_TIME_FMT);
		$query = "INSERT INTO `Activate` (`Login`, `HashPass`, `Key`, `Date`) VALUES ('{$login}', '{$hashpass}', '{$key}', '{$date}')";
		return mysqli_query(sqlLink(), $query);
	}

	function SendMail($to, $subject, $message)
	{
		$headers = "MIME-Version: 1.0\r\n";
		$headers .= "Content-type: text/html; charset=CP1251\r\n";
		$headers .= "From: service@diacomp.com";

		return mail($to, $subject, $message, $headers);
	}

	function SendRegMail($to, $key)
	{
		$subject = 'Регистрация в системе "Компенсация Онлайн"';
		$message = "Для подтверждения регистрации перейдите по ссылке: \n" .
				"http://diacomp.16mb.com/activate.php?key=" . $key;
		//return (mail($to, $subject, $message, $headers));
		return SendMail($to, $subject, $message);
	}

	function SendReportMail($to, $message)
	{
		$subject = "Compensation Bug Report";
		return SendMail($to, $subject, $message);
	}

	/**
	 * Разбивает текст на строки и возвращает в виде массива. Пустые строки
	 * игнорируются.
	 * @param string $text текст
	 * @return type массив строк
	 */
	function GetLines($text)
	{
		$result = array();
		$size = 0;
		$buf = "";

		for ($i = 0; $i < strlen($text); $i++)
		{
			if (($text{$i} == "\r") || ($text{$i} == "\n"))
			{
				if ($buf != "")
				{
					$result[$size++] = $buf;
					$buf = "";
				}
			}
			else
				$buf .= $text{$i};
		}

		if ($buf != "")
		{
			$result[$size] = $buf;
		}

		return $result;
	}

	/**
	 * Разбивает текст имеющимися в нём запятыми и возвращает в виде массива. Пустые слова
	 * игнорируются.
	 * @param string $text текст
	 * @return type массив строк
	 */
	function GetListColons($text)
	{
		$result = array();
		$size = 0;
		$buf = "";

		for ($i = 0; $i < strlen($text); $i++)
		{
			if ($text{$i} == ",")
			{
				if ($buf != "")
				{
					$result[$size++] = $buf;
					$buf = "";
				}
			}
			else
				$buf .= $text{$i};
		}

		if ($buf != "")
		{
			$result[$size] = $buf;
		}

		return $result;
	}

	// TODO: как объединить предыдущие две функции? И надо ли?

	function f00($val)
	{
		return ((strlen($val) == 1) ? "0" . $val : $val);
	}

	/**
	 * Переводит тип
	 * @param $destination Объект назначения
	 * @param stdClass $source Источник
	 */
	function Cast(stdClass $source, &$destination)
	{
		$sourceReflection = new \ReflectionObject($source);
		$sourceProperties = $sourceReflection->getProperties();
		foreach ($sourceProperties as $sourceProperty)
		{
			$name = $sourceProperty->getName();
			if (gettype($destination->{$name}) == "object")
			{
				Cast($source->$name, $destination->{$name});
			}
			else
			{
				$destination->{$name} = $source->$name;
			}
		}

		return $destination;
	}
?>