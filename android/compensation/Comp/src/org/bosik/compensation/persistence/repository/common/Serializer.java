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
 * @param <T>Тип объекта
 */
public interface Serializer<T>
{
	/**
	 * Загружает объект из строки
	 * 
	 * @param data
	 *            Cтрока, хранящая объект
	 * @return Объект
	 */
	public T read(String data);

	/**
	 * Сохраняет объект в строку
	 * 
	 * @param object
	 *            Объект
	 * @return Cтрока, хранящая объект
	 */
	public String write(T object);
}