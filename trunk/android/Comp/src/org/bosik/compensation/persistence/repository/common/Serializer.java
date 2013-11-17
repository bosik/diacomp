package org.bosik.compensation.persistence.repository.common;

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
	 * Загружает объект из строки
	 * 
	 * @param object
	 *            Объект (должен быть предварительно создан)
	 * @param data
	 *            Cтрока, хранящая объект
	 */
	public void read(T object, String data); // TODO: remove it

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