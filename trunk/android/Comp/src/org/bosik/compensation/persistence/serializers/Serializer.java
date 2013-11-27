package org.bosik.compensation.persistence.serializers;

import java.util.List;

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
	 * Создаёт объект из строки
	 * 
	 * @param data
	 *            Строка, хранящая объект
	 * @return Объект
	 */
	public T read(String data);

	public List<T> readAll(String data);

	/**
	 * Сохраняет объект в строку
	 * 
	 * @param object
	 *            Объект
	 * @return Cтрока, хранящая объект
	 */
	public String write(T object);

	public String writeAll(List<T> object);
}