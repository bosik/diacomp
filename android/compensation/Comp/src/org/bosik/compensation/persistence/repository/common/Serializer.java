package org.bosik.compensation.persistence.repository.common;

/**
 * Интерфейс, включающий в себя методы:
 * <ul>
 * <li>сериализация объекта в строку</li>
 * <li>десериализация объекта из строки</li>
 * </ul>
 * 
 * @author Bosik
 * 
 * @param <T>Тип базы
 */
public interface Serializer<T>
{
	/**
	 * Получает номер версии объекта
	 * 
	 * @param data
	 *            Cтрока, хранящая данные
	 * @return Номер версии
	 */
	// public int getVersion(String data);
	// TODO: cleanup

	/**
	 * Загружает объект из строки
	 * 
	 * @param data
	 *            Cтрока, хранящая объект
	 * @return База
	 */
	public T read(String data);

	/**
	 * Сохраняет базу в строку
	 * 
	 * @param base
	 *            База
	 * @return Cтрока, хранящая объект
	 */
	public String write(T base);
}