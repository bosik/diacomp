<?php

	//include_once 'DiaryPage.php';
	include_once 'common.php';
	include_once 'process.php';

	//$url_auth = "http://api.vk.com/oauth/authorize?client_id=1&redirect_uri=http://api.vk.com/blank.html&scope=262146&display=wap&response_type=token&_hash=0";
	//$url_auth = "http://api.vk.com/oauth/authorize?client_id=1&scope=262146&display=wap&response_type=token&_hash=0";
	//$url_getcomm = "https://api.vkontakte.ru/method/wall.getComments?post_id=3402&count=100&offset=100&owner_id=-12922665&access_token=9cb7b689d147fb999eafefd80d9e3e10fb99e3e9e3a10f88b329a09e9170601";
	//$url_auth = "http://api.vk.com/oauth/authorize";
	/* $fp = fsockopen($url_auth, 80, $errno, $errstr, 30);
	  if (!$fp) {
	  //print "$errstr ($errno)\n";
	  }
	  else
	  {
	  $out = "GET / HTTP/1.1\r\n";
	  $out .= "Host: " . $url_auth . "\r\n";
	  $out .= "Connection: Close\r\n\r\n";
	  fwrite($fp, $out);

	  while (!feof($fp))
	  {
	  print fgets($fp, 128);
	  }
	  fclose($fp);
	  } */

	//$myGetData = $url_auth;
	//print file_get_contents($url_getcomm);
	//print http_get($url_auth);

	/* $handle = fopen($url_auth, "r");
	  if (!$handle) {
	  print "error";
	  }
	  else
	  {
	  while (!feof($handle))
	  {
	  print fgets($handle, 128);
	  }
	  } */

	// GetPageJSON
	//		Сервер получает дату
	//		Сервер находит страницу в формате Plain
	//		Сервер конвертирует данные Plain -> JSON 
	// Сервер фомирует страницу с JSON
	// Клиент редактирует данные
	// Клиент отправляет данные в формате JSON на сервер
	// Сервер конвертирует данные JSON -> Plain
	// Сервер сохраняет данные

//	$user_id = 1;
//	$date = date(STD_DATE_FMT);
//	
//	$json = GetPageJSON($user_id, $date);
//	echo $json;
//
//	/* edit JSON here */
//
//	$page = new DiaryPage();
//	Cast(json_decode($json), $page);
//	$page->Encode('utf-8', 'windows-1251');
//	echo "\n\n";
//	PostPageJSON($user_id, $json);
	
	print gmdate(STD_TIME_FMT);
?>
