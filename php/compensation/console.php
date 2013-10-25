<?php
	require_once 'safe_slash.php';
	require_once 'common.php'; // <--- "session_start();" inside
	require_once 'process.php';
	require_once 'mysql.php';

	// Получение версии API - доступно всем
	if (isset($_GET['getAPIVersion']))
	{
		echo CURRENT_API_VERSION;
	}
	else

	// не авторизован
	if (!IsLogged())
	{
		echo 'Error: log in first';
	}
	else

	// авторизован
	{
		$cur_id = GetUserId();

		// ========================= ДНЕВНИК =================================
		// 1. получение списка страниц, изменённых после указанного времени
		// diary.getModList
		// time=1992-04-02 09:45:00

		if (isset($_GET['diary:getModList']) && isset($_GET['time']))
		{
			$time = mysqli_real_escape_string(sqlLink(), $_GET['time']);
			echo GetModList($cur_id, $time);
		}
		else

		// 2. Выгрузка страниц на сервер
		// diary:upload
		// pages=<page><page>...<page>
		if (isset($_POST['diary:upload']) && isset($_POST['pages']))
		{
			$pages = $_POST['pages'];
			
			if (isset($_POST['format']) && ($_POST['format'] == 'json'))
			{
				echo Answer(PostPageJSON($cur_id, $pages));
				//echo "JSON Upload; ID: ", $cur_id, "\n", "Pages: ", $pages;
			}
			else
			{
				//echo "<Console.php pages=" . $pages . "</Console.php>\n";
				echo Answer(UploadPages($cur_id, $pages));
			}
		}
		else

		// 3. Получение страниц с сервера
		// diary:download
		// dates=2009-11-18,2012-04-02
		if (isset($_GET['diary:download']) && isset($_GET['dates']))
		{
			if (isset($_GET['format']) && ($_GET['format'] == 'json'))
			{
				$dates = mysqli_real_escape_string(sqlLink(), $_GET['dates']);
				$datelist = GetListColons($dates);

				for ($i = 0; $i < count($datelist); $i++)
				{
					echo GetPageJSON($cur_id, $datelist{$i});
					break;
					// возвращаем только первую страницу
				}
			}
			else
			{
				$dates = mysqli_real_escape_string(sqlLink(), $_GET['dates']);
				$datelist = GetListColons($dates);

				for ($i = 0; $i < count($datelist); $i++)
				{
					echo GetPage($cur_id, $datelist{$i}, true);
				}
			}
		}
		else

		// ========================= БАЗА ПРОДУКТОВ =================================

		if (isset($_GET['foodbase:getVersion']))
		{
			echo GetFoodbaseVersion($cur_id);
		}
		else

		if (isset($_GET['foodbase:download']))
		{
			echo DownloadFoodbase($cur_id);
		}
		else

		if (isset($_POST['foodbase:upload']) && isset($_POST['version']) && isset($_POST['data']))
		{
			$version = mysqli_real_escape_string(sqlLink(), $_POST['version']);
			$data = mysqli_real_escape_string(sqlLink(), $_POST['data']);
			echo Answer(UploadFoodbase($cur_id, $data, $version));
		}
		else
			
		// ========================= БАЗА БЛЮД =================================
			
		if (isset($_GET['dishbase:getVersion']))
		{
			echo GetDishbaseVersion($cur_id);
		}
		else

		if (isset($_GET['dishbase:download']))
		{
			echo DownloadDishbase($cur_id);
		}
		else

		if (isset($_POST['dishbase:upload']) && isset($_POST['version']) && isset($_POST['data']))
		{
			$version = mysqli_real_escape_string(sqlLink(), $_POST['version']);
			$data = mysqli_real_escape_string(sqlLink(), $_POST['data']);
			echo Answer(UploadDishbase($cur_id, $data, $version));
		}
		else
			
		// ========================= КОЭФФИЦИЕНТЫ =================================

		if (isset($_GET['koofs:download']))
		{
			echo DownloadKoofs($cur_id);
		}
		else

		if (isset($_POST['koofs:upload']) && isset($_POST['data']))
		{
			$data = mysqli_real_escape_string(sqlLink(), $_POST['data']);
			echo Answer(UploadKoofs($cur_id, $data));
		}
		else
			
		// ========================= ОТЧЁТЫ ОБ ОШИБКАХ (ДЕМО) =================================

		if (isset($_POST['report']) && isset($_POST['msg']))
		{
			$to = "bosiknk@rambler.ru";
			$message = mysqli_real_escape_string(sqlLink(), $_POST['msg']);
			echo Answer(SendReportMail($to, $message));
		}
		else


		// ========================= ПОИСК ПРОДУКТОВ (ДЕМО) =================================
		if (isset($_GET['foodbase:search']) && isset($_GET['q']))
		{
			$key = mysqli_real_escape_string(sqlLink(), $_GET['q']);
			echo Search($key);
		}
		else

		// выведем информацию о версии
		{
			echo "<h2>Compensation Console</h2>\nAPI " . CURRENT_API_VERSION;
		}
	}
?>