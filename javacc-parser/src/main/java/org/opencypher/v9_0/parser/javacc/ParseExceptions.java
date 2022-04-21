/*
 * Copyright (c) Neo4j Sweden AB (http://neo4j.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.opencypher.v9_0.parser.javacc;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ParseExceptions extends RuntimeException {
    public static List<String> expected(int[][] expectedTokenSequences, String[] tokenImage) {
        Map<Integer, Long> tokenCount = Arrays.stream(expectedTokenSequences)
                .flatMapToInt(Arrays::stream)
                .boxed()
                .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
        List<String> strings = processExpectedList(tokenCount, tokenImage);
        Collections.sort(strings);
        return strings;
    }

    public static List<String> processExpectedList(Map<Integer, Long> expectedTokens, String[] tokenImage) {
        long identifiers = expectedTokens.getOrDefault(CypherConstants.IDENTIFIER, 0L);
        long plusCount = expectedTokens.getOrDefault(CypherConstants.PLUS, 0L);
        long expressions = Math.min(identifiers, plusCount);
        if (identifiers > 0) {
            filterTokenSet(expectedTokens, IdentifierTokens.getIdentifierTokens(), identifiers);
        }
        if (expressions > 0) {
            filterTokenSet(expectedTokens, ExpressionTokens.getExpressionTokens(), expressions);
        }
        List<String> expectedMessage = expectedTokens.keySet().stream()
                .map(token -> {
                    String image = tokenImage[token];
                    return image.equals("\"$\"") ? "a parameter" : image;
                })
                .collect(Collectors.toList());
        if (identifiers - expressions > 0) {
            expectedMessage.add("an identifier");
        }
        if (expressions > 0) {
            expectedMessage.add("an expression");
        }
        return expectedMessage;
    }

    private static Map<Integer, Long> filterTokenSet(
            Map<Integer, Long> expectedTokens, Set<Integer> tokens, long quantitiy) {
        for (Integer token : tokens) {
            if (expectedTokens.containsKey(token)) {
                long newCount = expectedTokens.get(token) - quantitiy;
                if (newCount > 0) {
                    expectedTokens.replace(token, newCount);
                } else {
                    expectedTokens.remove(token);
                }
            }
        }
        return expectedTokens;
    }
}
