using System;
using System.Collections.Generic;

namespace System.Linq
{
    public static class OldEnumerable
    {
        public static TSource LastOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new ArgumentNullException("source");
            if (predicate == null) throw new ArgumentNullException("predicate");
            TSource result = default(TSource);
            foreach (TSource element in source) {
                if (predicate(element)) {
                    result = element;
                }
            }
            return result;
        }

        public static TSource FirstOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new Exception("source");
            if (predicate == null) throw new Exception("predicate");
            foreach (TSource element in source) {
                if (predicate(element)) return element;
            }
            return default(TSource);
        }

        public static TSource SingleOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new Exception("source");
            if (predicate == null) throw new Exception("predicate");
            TSource result = default(TSource);
            long count = 0;
            foreach (TSource element in source) {
                if (predicate(element)) {
                    result = element;
                    checked { count++; }
                }
            }
            switch (count) {
                case 0: return default(TSource);
                case 1: return result;
            }
            throw new Exception();
        }
    }
}
