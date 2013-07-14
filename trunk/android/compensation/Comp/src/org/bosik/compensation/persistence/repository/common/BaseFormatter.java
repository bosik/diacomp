package org.bosik.compensation.persistence.repository.common;

/**
 * Интерфейс, включающий в себя методы:
 * <ul>
 * <li>загрузка базы из xml-строки</li>
 * <li>сохранение базы в xml-строку</li>
 * <li>получение версии базы по xml-строке</li>
 * </ul>
 * 
 * @author Bosik
 * 
 * @param <T>Тип базы
 */
public interface BaseFormatter<T>
{
	/**
	 * Получает номер версии базы
	 * @param xmlData xml-строка, хранящая базу
	 * @return Номер версии
	 */
	public int getVersion(String xmlData);

	/**
	 * Загружает базу из строки
	 * @param xmlData xml-строка, хранящая базу
	 * @return База
	 */
	public T read(String xmlData);

	/**
	 * Сохраняет базу в строку
	 * @param base База
	 * @return xml-строка, хранящая базу
	 */
	public String write(T base);
}