package org.bosik.compensation.persistence.repository;

import org.bosik.compensation.persistence.repository.common.LocalBaseRepository;
import org.bosik.compensation.persistence.repository.diary.LocalDiaryRepository;
import org.bosik.compensation.persistence.repository.diary.WebDiaryRepository;
import org.bosik.compensation.persistence.repository.providers.WebClient;

/**
 * Класс-singleton, хранит дневники (DiaryRepository) и базы данных
 * 
 * @author Bosik
 */
public class Storage
{
	// отладка

	@SuppressWarnings("unused")
	private static final String TAG = "Storage";

	// компоненты (в качестве типа указана конкретная реализация - требуется для внешней настройки)

	public static WebClient web_client = null;
	
	public static LocalDiaryRepository local_diary = null;
	public static WebDiaryRepository web_diary = null;
	
	public static LocalBaseRepository local_foodbase = null;
	
	// разное

	// private static final int CONNECTION_TIMEOUT = 6 * 1000; // TODO: move to string resources

	/**
	 * Инициализирует дневники. Метод можно вызывать повторно.
	 * 
	 * @param resolver
	 */
	/*
	 * public static void init(ContentResolver resolver) {
	 * 
	 * }
	 */
}