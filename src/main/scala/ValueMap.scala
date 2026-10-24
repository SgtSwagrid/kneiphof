package com.alecdorrington
package kneiphof

/**
  * An immutable map from keys of type [[K]], compared only by equality, to
  * values of type [[A]].
  *
  * Unlike [[Map]], a value map is covariant in its keys, so a
  * `ValueMap[Int, A]` is also a `ValueMap[Any, A]`. Lookups accept any key for
  * the same reason. It's stored as a hash trie, so lookups and updates take
  * effectively constant time.
  */
final class ValueMap[+K, +A] private (entries: Map[Any, (K, A)])
  extends Iterable[(K, A)]:

  override def iterator: Iterator[(K, A)] = entries.valuesIterator

  override def knownSize: Int = entries.size

  /** The keys of this map. */
  def keys: Iterable[K] = entries.values.map(_(0))

  /** The values of this map. */
  def values: Iterable[A] = entries.values.map(_(1))

  /** @return `true` if and only if this map contains [[key]]. */
  def contains(key: Any): Boolean = entries.contains(key)

  /** @return The value for [[key]], if there is one. */
  def get(key: Any): Option[A] = entries.get(key).map(_(1))

  /** @return The value for [[key]], or [[default]] if there isn't one. */
  def getOrElse[B >: A](key: Any, default: => B): B =
    get(key).getOrElse(default)

  /** @return The key of this map equal to [[key]], if there is one. */
  def getKey(key: Any): Option[K] = entries.get(key).map(_(0))

  /** @return This map with [[key]] mapped to [[value]]. */
  def updated[L >: K, B >: A](key: L, value: B): ValueMap[L, B] =
    new ValueMap(entries.updated(key, (key, value)))

  /** @return This map without [[key]]. */
  def removed(key: Any): ValueMap[K, A] = new ValueMap(entries - key)

  override def equals(that: Any): Boolean = that match
    case that: ValueMap[?, ?] => size == that.size &&
      forall((key, value) => that.get(key).contains(value))
    case _ => false

  override def hashCode: Int = entries.view.mapValues(_(1)).toMap.##

  override def toString: String = map((key, value) => s"$key -> $value")
    .mkString("ValueMap(", ", ", ")")

/** The companion object for [[ValueMap]]. */
object ValueMap:

  /** @return The map with no entries. */
  def empty[K, A]: ValueMap[K, A] = new ValueMap(Map.empty)
